#include <compare>
#include <iostream>
#include <fstream>

#include "constants.hpp"
#include "debugging.hpp"

using namespace std;
using namespace torch;

constexpr int64_t SENTINELS = 2; // empty, out-of-bounds
constexpr int64_t NUM_PERMUTATIONS = 6;
constexpr int64_t INDEX_DIM = 0, ONEHOT_DIM = 1, CONV_WIDTH_DIM = 2, CONV_HEIGHT_DIM = 3;
const int64_t COLOR_OUT_OF_BOUNDS = COLORS + 1, SHAPE_OUT_OF_BOUNDS = SHAPES + 1;

// ideally we'd use kBool, but convolutions aren't implemented for those
// see also https://github.com/pytorch/pytorch/issues/136578
const TensorOptions GPU_BOOL_REP = TensorOptions().dtype(kF16).device(kCUDA);
typedef float CXX_BOOL_REP;

const Tensor PERMUTATIONS = torch::tensor({0,1,2,3,4,0,2,1,3,4,1,0,2,3,4,1,2,0,3,4,2,0,1,3,4,2,1,0,3,4}).reshape({NUM_PERMUTATIONS,COLORS+SENTINELS});

class Boards {
	public:
		// will read 128 bytes (16 8-byte chunks) from base_board:
		// 	* the empty cell is represented by the byte 0b00010011 (= 0x13 = SHAPES << 2 | COLORS)
		// 	* bottom two bits are color
		// 	* next two bits are shape
		// (we could in principle pack two cells into each byte, but the extra
		// complications in encoding and decoding don't seem worth it)
		//
		// diffs is a sequence of modifications, with each modification being a
		// sequence of position/cell pairs:
		// 	* 0xff ends a modification
		// 	* 0xfe ends the whole sequence of modifications
		// 	* otherwise two bytes are read, with the first being interpreted as
		// 	  a position and the second as a cell to replace whatever's currently
		// 	  in that position:
		// 	  	* a position byte uses the upper nibble for x and lower nibble
		// 	  	  for y
		// 	  	* a cell byte is encoded as with the board (starting from the
		// 	  	  bottom bit, 2 bits for color, 3 bits for shape, 1 bit for
		// 	  	  emptiness)
		// if you want the base board included in the set, you must explicitly
		// send an empty modification
		// does absolutely zero error checking
		Boards(char *base_board, char *diffs);

		int64_t size() const { return color_.size(INDEX_DIM); }

		// these are padded appropriately for the given convolution size
		const Tensor p_color(int64_t conv_width, int64_t conv_height) const;
		const Tensor p_shape(int64_t conv_width, int64_t conv_height) const;

		string sketch() const;
		friend ostream &operator<<(ostream &o, const Boards &g);

	protected:
		void generate_padding_cache(int64_t conv_width, int64_t conv_height) const;

		// num boards x ((COLORS or SHAPES) + SENTINELS) x BOARD_WIDTH x BOARD_HEIGHT
		Tensor color_, shape_;
		// padded versions of color_ and shape_, together with the conv_width and conv_height they're padded for
		mutable int64_t p_conv_width_, p_conv_height_;
		mutable Tensor p_color_, p_shape_;
};

class obitstream {
	public:
		obitstream(ostream &s) : s_(s), next_byte_(0), next_bit_(0) {}
		~obitstream();
		obitstream &operator<<(bool b);

	protected:
		ostream &s_;
		uint8_t next_byte_;
		int next_bit_;
};

class ibitstream {
	public:
		ibitstream(istream &s) : s_(s), next_byte_(0), next_bit_(0) {}
		bool get();

	protected:
		istream &s_;
		char next_byte_;
		int next_bit_;
};

class Chromosome {
	public:
		Chromosome(int64_t conv_width, int64_t conv_height, int64_t num_patterns = 0, float p = 0.5);
		Chromosome(const Tensor &color_pattern, const Tensor &shape_pattern, const Tensor &pattern_score);
		Chromosome clone() const;

		Tensor evaluate(const Boards &bs) const;

		int64_t size() const { return color_pattern_.size(INDEX_DIM); }
		int64_t conv_width() const { return color_pattern_.size(CONV_WIDTH_DIM); }
		int64_t conv_height() const { return color_pattern_.size(CONV_HEIGHT_DIM); }

		bool get_color_pattern(int64_t pattern, int64_t color, int64_t w, int64_t h) const;
		bool get_shape_pattern(int64_t pattern, int64_t shape, int64_t w, int64_t h) const;
		float get_pattern_score(int64_t pattern) const;
		string encode_patterns() const;

		void set_color_pattern(int64_t pattern, int64_t color, int64_t w, int64_t h, bool v);
		void set_shape_pattern(int64_t pattern, int64_t color, int64_t w, int64_t h, bool v);
		void set_pattern_score(int64_t pattern, float v);
		void decode_patterns(string ps);

		const Tensor &p_color_pattern() const;
		const Tensor &p_shape_pattern() const;
		const Tensor &p_pattern_score() const;

		Chromosome indices(vector<int64_t> is) const;
		Chromosome operator+(const Chromosome &other) const;
		Chromosome &operator+=(const Chromosome &other);

		void encode_scores(ostream &s) const;
		void encode_patterns(obitstream &s) const;
		void decode_scores(istream &s);
		void decode_patterns(ibitstream &s);

		string sketch() const;
		friend ostream &operator<<(ostream &o, const Chromosome &g);

	protected:
		void normalize_scores();
		static void assert_compatible(const Tensor &t, const TensorOptions &o);

		// patterns have a 1 where that color/shape is forbidden and a 0 where
		// it's allowed (chosen this way so that the convolution operator is
		// exactly the right operation for telling whether the pattern matches
		// at each position)
		// num patterns x (COLORS + SENTINELS) x conv width x conv height @ GPU_BOOL_REP
		Tensor color_pattern_;
		// num patterns x (SHAPES + SENTINELS) x conv width x conv height @ GPU_BOOL_REP
		Tensor shape_pattern_;
		// num patterns @ GPU_FLOAT
		Tensor pattern_score_;
		// These variants are 6x as long (in the first dimension) -- one extra
		// copy per color permutation. They may be undefined -- use the
		// accessor functions with the similar name to always get something
		// defined.
		mutable Tensor p_color_pattern_, p_shape_pattern_, p_pattern_score_;
};

class ConvolutionSize {
	public:
		ConvolutionSize(int64_t w, int64_t h);
		ConvolutionSize(const Chromosome &c) : ConvolutionSize(c.conv_width(), c.conv_height()) {}
		explicit ConvolutionSize(uint8_t encoding) : sz_(encoding) { assert(valid(sz_)); }

		int64_t width() const { return 1 + ((sz_ & kWidthMask) >> kWidthShift); }
		int64_t height() const { return 1 + ((sz_ & kHeightMask) >> kHeightShift); }

		// become the larger width of the two, and the larger height of the two
		void join(const ConvolutionSize &other);

		strong_ordering operator<=>(const ConvolutionSize &other) const = default;

		uint8_t encode() const { return sz_; }

		static bool valid(uint8_t c) { return !(c & kInvalidMask); }

	protected:
		uint8_t sz_;
		static constexpr uint8_t kWidthMask = 0x70, kHeightMask = 0x0f, kInvalidMask = 0x80;
		static constexpr int kWidthShift = 4, kHeightShift = 0;

	public:
		static constexpr uint8_t kInvalidEncoding = kInvalidMask;
};

class Genome {
	public:
		Genome() {}
		Genome clone() const;

		Genome operator+(const Genome &other) const;
		Tensor evaluate(const Boards &bs) const;

		vector<ConvolutionSize> sizes() const;
		Chromosome &get_chromosome(int64_t w, int64_t h);
		void set_chromosome(const Chromosome &c);
		Genome &operator+=(const Chromosome &c);

		void encode(ostream &s) const;
		static Genome decode(istream &s);

		string sketch() const;
		friend ostream &operator<<(ostream &o, const Genome &g);

	protected:
		map<ConvolutionSize, Chromosome> chromosomes_;
};

Boards::Boards(char *base_board, char *diffs)
	: p_conv_width_(0), p_conv_height_(0)
{
	int num_boards = 0, i = 0;
	while(diffs[i] != '\xfe') {
		if(diffs[i] == '\xff') ++num_boards;
		else ++i;
		++i;
	}

	Tensor base_color = torch::zeros({COLORS+SENTINELS, BOARD_WIDTH, BOARD_HEIGHT}, GPU_BOOL_REP);
	Tensor base_shape = torch::zeros({SHAPES+SENTINELS, BOARD_WIDTH, BOARD_HEIGHT}, GPU_BOOL_REP);

#define MASK_AND_SHIFT(ptr, cat) ((*ptr & k ## cat ## Mask) >> k ## cat ## Shift)
	constexpr char kColorMask = 0b00011, kShapeMask = 0b11100, kXMask = 0b01110000, kYMask = 0b00001111;
	constexpr int kColorShift = 0, kShapeShift = 2, kXShift = 4, kYShift = 0;

	for(int y = 0; y < BOARD_HEIGHT; ++y) {
		for(int x = 0; x < BOARD_WIDTH; ++x, ++base_board) {
			base_color[MASK_AND_SHIFT(base_board, Color)][x][y] = 1;
			base_shape[MASK_AND_SHIFT(base_board, Shape)][x][y] = 1;
		}
	}

	color_ = base_color.expand({num_boards, -1, -1, -1}).clone();
	shape_ = base_shape.expand({num_boards, -1, -1, -1}).clone();

	i = 0;
	while(*diffs != '\xfe') {
		if(*diffs == '\xff') {
			++i;
			++diffs;
			continue;
		}

		int x = MASK_AND_SHIFT(diffs, X), y = MASK_AND_SHIFT(diffs, Y);
		color_.index_put_({i, indexing::Slice(), x, y}, 0);
		shape_.index_put_({i, indexing::Slice(), x, y}, 0);
		++diffs;

		color_[i][MASK_AND_SHIFT(diffs, Color)][x][y] = 1;
		shape_[i][MASK_AND_SHIFT(diffs, Shape)][x][y] = 1;
		++diffs;
	}
#undef MASK_AND_SHIFT
}

void Boards::generate_padding_cache(int64_t w, int64_t h) const {
	w = std::max(p_conv_width_, w);
	h = std::max(p_conv_height_, h);

	int64_t x = (w-1)/2, y = (h-1)/2;
	indexing::Slice all, board_x(x, x+BOARD_WIDTH), board_y(y, y+BOARD_HEIGHT);
	p_conv_width_ = w;
	p_conv_height_ = h;
	p_color_ = torch::zeros({size(), COLORS+SENTINELS, BOARD_WIDTH + w-1, BOARD_HEIGHT + h-1}, GPU_BOOL_REP);
	p_shape_ = torch::zeros({size(), SHAPES+SENTINELS, BOARD_WIDTH + w-1, BOARD_HEIGHT + h-1}, GPU_BOOL_REP);
	p_color_.index_put_({all, COLOR_OUT_OF_BOUNDS, all, all}, 1);
	p_shape_.index_put_({all, SHAPE_OUT_OF_BOUNDS, all, all}, 1);
	p_color_.index_put_({all, all, board_x, board_y}, color_);
	p_shape_.index_put_({all, all, board_x, board_y}, shape_);
}

indexing::Slice recenter(int64_t length, int64_t cache_sz, int64_t req_sz) {
	int64_t offset = (cache_sz-1)/2 - (req_sz-1)/2;
	return indexing::Slice(offset, offset+length+req_sz-1);
}

const Tensor Boards::p_color(int64_t w, int64_t h) const {
	if(!p_color_.defined() || p_conv_width_ < w || p_conv_height_ < h) {
		// we sort of assume that if you're asking for p_color(), you're about
		// to ask for p_shape(), and generate 'em both
		generate_padding_cache(w, h);
	}
	return p_color_.index({
		indexing::Slice(), // number of boards
		indexing::Slice(), // color
		recenter(BOARD_WIDTH, p_conv_width_, w),
		recenter(BOARD_HEIGHT, p_conv_height_, h),
	});
}

const Tensor Boards::p_shape(int64_t w, int64_t h) const {
	if(!p_shape_.defined() || p_conv_width_ < w || p_conv_height_ < h) {
		// we sort of assume that if you're asking for p_shape(), you're about
		// to ask for p_color(), and generate 'em both
		generate_padding_cache(w, h);
	}
	return p_shape_.index({
		indexing::Slice(), // number of boards
		indexing::Slice(), // shape
		recenter(BOARD_WIDTH, p_conv_width_, w),
		recenter(BOARD_HEIGHT, p_conv_height_, h),
	});
}

string Boards::sketch() const {
	stringstream o;
	o << "{ color: " << TensorSketch(color_) << ", shape: " << TensorSketch(shape_);
	if(p_color_.defined())
		o << ", padded color: (" << p_conv_width_ << "x" << p_conv_height_ << ") => " << TensorSketch(p_color_);
	if(p_shape_.defined())
		o << ", padded shape: (" << p_conv_width_ << "x" << p_conv_height_ << ") => " << TensorSketch(p_shape_);
	o << " }";
	return o.str();
}

ostream &operator<<(ostream &o, const Boards &bs) {
	string prefix;
	o << "Board { padding cache = {";
	if(bs.p_color_.defined()) { o << prefix << "color"; prefix = ", "; }
	if(bs.p_shape_.defined()) { o << prefix << "shape"; prefix = ", "; }
	o << "}, color = " << bs.color_ << ", shape = " << bs.shape_ << " }";
	return o;
}

Chromosome::Chromosome(int64_t w, int64_t h, int64_t n, float p) {
	color_pattern_ = (torch::rand({n, COLORS + SENTINELS, w, h}, GPU_FLOAT) < p).to(GPU_BOOL_REP);
	shape_pattern_ = (torch::rand({n, SHAPES + SENTINELS, w, h}, GPU_FLOAT) < p).to(GPU_BOOL_REP);
	pattern_score_ = torch::randn({n}, GPU_FLOAT);
	normalize_scores();

	assert(!color_pattern_.requires_grad());
	assert(!shape_pattern_.requires_grad());
	assert(!pattern_score_.requires_grad());
}

Chromosome::Chromosome(const Tensor &co, const Tensor &sh, const Tensor &sc)
	: color_pattern_(co), shape_pattern_(sh), pattern_score_(sc)
{
	assert_compatible(co, GPU_BOOL_REP);
	assert_compatible(sh, GPU_BOOL_REP);
	assert_compatible(sc, GPU_FLOAT);

	assert(co.dim() == 4);
	assert(sh.dim() == 4);
	assert(sc.dim() == 1);

	assert(co.size(INDEX_DIM) == sh.size(INDEX_DIM));
	assert(co.size(INDEX_DIM) == sc.size(INDEX_DIM));
	assert(co.size(ONEHOT_DIM) == COLORS + SENTINELS);
	assert(sh.size(ONEHOT_DIM) == SHAPES + SENTINELS);
	assert(co.size(CONV_WIDTH_DIM) == sh.size(CONV_WIDTH_DIM));
	assert(co.size(CONV_HEIGHT_DIM) == sh.size(CONV_HEIGHT_DIM));

	normalize_scores();

	assert(!color_pattern_.requires_grad());
	assert(!shape_pattern_.requires_grad());
	assert(!pattern_score_.requires_grad());
}

Chromosome Chromosome::clone() const {
	Chromosome result(color_pattern_.clone(), shape_pattern_.clone(), pattern_score_.clone());
	// we always set these fields back to Tensor() before modifying them, so no need to clone
	result.p_color_pattern_ = p_color_pattern_;
	result.p_shape_pattern_ = p_shape_pattern_;
	result.p_pattern_score_ = p_pattern_score_;
	return result;
}

bool Chromosome::get_color_pattern(int64_t pattern, int64_t color, int64_t w, int64_t h) const {
	return color_pattern_[pattern][color][w][h].item<CXX_BOOL_REP>() != 0;
}

bool Chromosome::get_shape_pattern(int64_t pattern, int64_t shape, int64_t w, int64_t h) const {
	return shape_pattern_[pattern][shape][w][h].item<CXX_BOOL_REP>() != 0;
}

float Chromosome::get_pattern_score(int64_t pattern) const {
	return pattern_score_[pattern].item<float>();
}

string Chromosome::encode_patterns() const {
	string result((size()*(COLORS + SENTINELS + SHAPES + SENTINELS)*conv_width()*conv_height()+7)/8, '\0');
	int bit = 0, byte = 0;
	for(int pattern = 0; pattern < size(); ++pattern) {
		for(int x = 0; x < conv_width(); ++x) {
			for(int y = 0; y < conv_height(); ++y) {
				for(int color = 0; color < COLORS + SENTINELS; ++color) {
					if(color_pattern_[pattern][color][x][y].item<CXX_BOOL_REP>() != 0)
						result[byte] |= 1 << bit;
					++bit;
					if(bit >= 8) {
						++byte;
						bit -= 8;
					}
				}
				for(int shape = 0; shape < SHAPES + SENTINELS; ++shape) {
					if(shape_pattern_[pattern][shape][x][y].item<CXX_BOOL_REP>() != 0)
						result[byte] |= 1 << bit;
					++bit;
					if(bit >= 8) {
						++byte;
						bit -= 8;
					}
				}
			}
		}
	}
	return result;
}

void Chromosome::set_color_pattern(int64_t pattern, int64_t color, int64_t w, int64_t h, bool v) {
	color_pattern_[pattern][color][w][h] = v;
	p_color_pattern_ = Tensor();
}

void Chromosome::set_shape_pattern(int64_t pattern, int64_t shape, int64_t w, int64_t h, bool v) {
	shape_pattern_[pattern][shape][w][h] = v;
	p_shape_pattern_ = Tensor();
}

void Chromosome::set_pattern_score(int64_t pattern, float v) {
	pattern_score_[pattern] = v;
	normalize_scores(); // this clears p_pattern_score_
}

void Chromosome::decode_patterns(string ps) {
	int bit = 0, byte = 0;
	for(int pattern = 0; pattern < size(); ++pattern) {
		for(int x = 0; x < conv_width(); ++x) {
			for(int y = 0; y < conv_height(); ++y) {
				for(int color = 0; color < COLORS + SENTINELS; ++color) {
					color_pattern_[pattern][color][x][y] = byte < ps.size()
						? (ps[byte] >> bit) & 1
						: 0;
					++bit;
					if(bit >= 8) {
						++byte;
						bit -= 8;
					}
				}
				for(int shape = 0; shape < SHAPES + SENTINELS; ++shape) {
					shape_pattern_[pattern][shape][x][y] = byte < ps.size()
						? (ps[byte] >> bit) & 1
						: 0;
					++bit;
					if(bit >= 8) {
						++byte;
						bit -= 8;
					}
				}
			}
		}
	}
	p_color_pattern_ = Tensor();
	p_shape_pattern_ = Tensor();
}

const Tensor &Chromosome::p_color_pattern() const {
	if(!p_color_pattern_.defined()) {
		const int64_t sz = size();
		p_color_pattern_ = torch::zeros({NUM_PERMUTATIONS*sz, COLORS+SENTINELS, conv_width(), conv_height()}, GPU_BOOL_REP);
		for(int i = 0; i < NUM_PERMUTATIONS; ++i) {
			p_color_pattern_.index_put_
				( {indexing::Slice(i*sz, (i+1)*sz), "..."}
				, color_pattern_.index({indexing::Slice(), PERMUTATIONS[i], "..."})
				);
		}
	}
	return p_color_pattern_;
}

const Tensor &Chromosome::p_shape_pattern() const {
	if(!p_shape_pattern_.defined()) {
		p_shape_pattern_ = shape_pattern_
			.expand({NUM_PERMUTATIONS, -1, -1, -1, -1})
			.reshape({NUM_PERMUTATIONS*size(), SHAPES+SENTINELS, conv_width(), conv_height()});
	}
	return p_shape_pattern_;
}

const Tensor &Chromosome::p_pattern_score() const {
	if(!p_pattern_score_.defined()) {
		p_pattern_score_ = pattern_score_
			.expand({NUM_PERMUTATIONS, size()})
			.reshape({NUM_PERMUTATIONS*size()});
	}
	return p_pattern_score_;
}

Chromosome Chromosome::indices(vector<int64_t> is) const {
	Tensor tis = torch::tensor(is);
	return Chromosome(color_pattern_.index({tis, "..."}), shape_pattern_.index({tis, "..."}), pattern_score_.index({tis}));
}

Chromosome Chromosome::operator+(const Chromosome &other) const {
	return Chromosome
		( cat({color_pattern_, other.color_pattern_})
		, cat({shape_pattern_, other.shape_pattern_})
		, cat({pattern_score_, other.pattern_score_})
		);
}

Chromosome &Chromosome::operator+=(const Chromosome &other) {
	color_pattern_ = cat({color_pattern_, other.color_pattern_});
	shape_pattern_ = cat({shape_pattern_, other.shape_pattern_});
	pattern_score_ = cat({pattern_score_, other.pattern_score_});

	p_color_pattern_ = Tensor();
	p_shape_pattern_ = Tensor();
	p_pattern_score_ = Tensor();

	return *this;
}

string Chromosome::sketch() const {
	stringstream o;

	o << "{ color: " << TensorSketch(color_pattern_);
	o << ", shape: " << TensorSketch(shape_pattern_);
	o << ", score: " << TensorSketch(pattern_score_);
	if(p_color_pattern_.defined()) o << ", color cache: " << TensorSketch(p_color_pattern_);
	if(p_shape_pattern_.defined()) o << ", shape cache: " << TensorSketch(p_shape_pattern_);
	if(p_pattern_score_.defined()) o << ", score cache: " << TensorSketch(p_pattern_score_);
	o << " }";

	return o.str();
}

ostream &operator<<(ostream &o, const Chromosome &g) {
	string prefix;
	o << "Chromosome {size = " << g.size() << ", permutation cache = {";
	if(g.p_color_pattern_.defined()) { o << prefix << "color"; prefix = ", "; }
	if(g.p_shape_pattern_.defined()) { o << prefix << "shape"; prefix = ", "; }
	if(g.p_pattern_score_.defined()) { o << prefix << "score"; prefix = ", "; }
	o << "}";

	prefix = "";
	for(int i = 0; i < g.size(); ++i) {
		o << ",\npattern " << i << " = { ";
		o << "score = " << g.pattern_score_[i].item<float>() << ", " << endl;
		o << "color = " << g.color_pattern_[i] << "," << endl;
		o << "shape = " << g.shape_pattern_[i] << endl << "}";
		prefix = "\n";
	}
	o << prefix << "}";
	return o;
}

void Chromosome::normalize_scores() {
	if(size() <= 0) return;
	pattern_score_ /= pattern_score_.abs().max();
	p_pattern_score_ = Tensor();
}

void Chromosome::assert_compatible(const Tensor &t, const TensorOptions &o) {
	assert(t.dtype() == o.dtype());
	assert(t.device().type() == o.device().type());
}

Tensor Chromosome::evaluate(const Boards &bs) const {
	const int64_t cw = conv_width(), ch = conv_height();
	Tensor mismatch_color = conv2d(bs.p_color(cw, ch), p_color_pattern()),
	       mismatch_shape = conv2d(bs.p_shape(cw, ch), p_shape_pattern());
	Tensor match = ((mismatch_color + mismatch_shape) == 0).to(GPU_BYTE);
	return (match.sum({2,3})*p_pattern_score()).sum({1});
}

void Chromosome::encode_scores(ostream &s) const {
	// must name it to make sure the underlying data pointer stays valid
	Tensor contiguous_scores = pattern_score_.to(kCPU).contiguous();
	char *scores = reinterpret_cast<char *>(contiguous_scores.data_ptr<float>());
	for(int i = 0; i < size() * sizeof(float); ++i)
		s << scores[i];
}

void Chromosome::encode_patterns(obitstream &s) const {
	for(int pattern = 0; pattern < size(); ++pattern) {
		for(int x = 0; x < conv_width(); ++x) {
			for(int y = 0; y < conv_height(); ++y) {
				for(int color = 0; color < COLORS + SENTINELS; ++color)
					s << (color_pattern_[pattern][color][x][y].item<CXX_BOOL_REP>() != 0);
				for(int shape = 0; shape < SHAPES + SENTINELS; ++shape)
					s << (shape_pattern_[pattern][shape][x][y].item<CXX_BOOL_REP>() != 0);
			}
		}
	}
}

void Chromosome::decode_scores(istream &s) {
	float scores[size()];
	for(int i = 0; i < sizeof(scores); ++i)
		s.get(reinterpret_cast<char *>(scores)[i]);
	pattern_score_ = torch::from_blob(scores, {size()}, CPU_FLOAT).to(kCUDA).clone();
	assert(all((pattern_score_ <= 1) * (pattern_score_ >= -1)).item<bool>());
	assert(any((pattern_score_ == 1) + (pattern_score_ == -1)).item<bool>());
	p_pattern_score_ = Tensor();
}

void Chromosome::decode_patterns(ibitstream &s) {
	for(int pattern = 0; pattern < size(); ++pattern) {
		for(int x = 0; x < conv_width(); ++x) {
			for(int y = 0; y < conv_height(); ++y) {
				for(int color = 0; color < COLORS + SENTINELS; ++color)
					color_pattern_[pattern][color][x][y] = s.get();
				for(int shape = 0; shape < SHAPES + SENTINELS; ++shape)
					shape_pattern_[pattern][shape][x][y] = s.get();
			}
		}
	}
}

ConvolutionSize::ConvolutionSize(int64_t w, int64_t h) {
	assert(!(--w & ~(kWidthMask >> kWidthShift)));
	assert(!(--h & ~(kHeightMask >> kHeightShift)));
	sz_ = (w << kWidthShift) | (h << kHeightShift);
}

void ConvolutionSize::join(const ConvolutionSize &other) {
	sz_ = std::max(sz_ & kWidthMask, other.sz_ & kWidthMask)
		| std::max(sz_ & kHeightMask, other.sz_ & kHeightMask)
		;
}

ConvolutionSize join(const std::vector<ConvolutionSize> &sizes) {
	ConvolutionSize ret = ConvolutionSize(0);
	for(const auto &size : sizes) ret.join(size);
	return ret;
}

ostream &operator<<(ostream &o, const ConvolutionSize &sz) {
	return o << sz.width() << "x" << sz.height();
}

Genome Genome::clone() const {
	Genome ret;
	map<ConvolutionSize, Chromosome>::const_iterator this_it = chromosomes_.begin();
	map<ConvolutionSize, Chromosome>::iterator ret_it = ret.chromosomes_.begin();
	while(this_it != chromosomes_.end()) {
		if(this_it->second.size())
			ret_it = ret.chromosomes_.insert(ret_it, pair(this_it->first, this_it->second.clone()));
		++this_it;
	}
	return ret;
}

Genome Genome::operator+(const Genome &other) const {
	Genome ret(other.clone());
	map<ConvolutionSize, Chromosome>::const_iterator src = chromosomes_.begin();
	map<ConvolutionSize, Chromosome>::iterator dst = ret.chromosomes_.begin();
	while(src != chromosomes_.end())
		if(src->second.size()) {
			dst = ret.chromosomes_.insert(dst, pair(src->first, src->second.clone()));
			++src;
		}
	return ret;
}

Tensor Genome::evaluate(const Boards &bs) const {
	ConvolutionSize max_size = join(sizes());
	bs.p_color(max_size.width(), max_size.height());
	// Semantically, we should make a second call here:
	//     bs.p_shape(max_size.width(), max_size.height());
	// But since we're already relying on internal implementation details by
	// pre-requesting/generating the padding caches, we go ahead and rely on
	// another internal detail: either call generates the cache for the other.

	Tensor out = torch::zeros({bs.size()}, GPU_FLOAT);
	for(auto [_, c] : chromosomes_) out += c.evaluate(bs);
	return out;
}

vector<ConvolutionSize> Genome::sizes() const {
	vector<ConvolutionSize> out; out.reserve(chromosomes_.size());
	for(const auto &[sz, c] : chromosomes_)
		if(c.size())
			out.push_back(sz);
	return out;
}

Chromosome &Genome::get_chromosome(int64_t w, int64_t h) {
	return chromosomes_.try_emplace(ConvolutionSize(w, h), w, h).first->second;
}

void Genome::set_chromosome(const Chromosome &c) {
	chromosomes_.erase(c);
	if(c.size()) chromosomes_.emplace(c, c.clone());
}

Genome &Genome::operator+=(const Chromosome &c) {
	map<ConvolutionSize, Chromosome>::iterator it = chromosomes_.find(c);
	if(it == chromosomes_.end())
		if(c.size()) chromosomes_.emplace(c, c.clone());
	else it->second += c;
	return *this;
}

void Genome::encode(ostream &s) const {
	for(const auto &[sz, c] : chromosomes_) {
		int64_t n = c.size();
		if(!n) continue;
		s << sz.encode();
		while(n > 0x7f) {
			s << uint8_t(0x80 | n);
			n >>= 7;
		}
		s << uint8_t(n);
		c.encode_scores(s);
	}
	s << ConvolutionSize::kInvalidEncoding;
	obitstream bits(s);
	for(const auto &[sz, c] : chromosomes_) {
		if(!c.size()) continue;
		c.encode_patterns(bits);
	}
}

Genome Genome::decode(istream &s) {
	Genome ret;
	auto it = ret.chromosomes_.begin();
	char sz_;

	while(s.get(sz_) && ConvolutionSize::valid(sz_)) {
		int64_t shift = 0, n = 0;
		char partial_n;
		while(s.get(partial_n) && (partial_n & 0x80) && shift <= 35) {
			n |= int64_t(partial_n & 0x7f) << shift;
			shift += 7;
		}
		n |= int64_t(partial_n) << shift;
		assert(n);
		assert(shift <= 35);
		ConvolutionSize sz(sz_);
		it = ret.chromosomes_.emplace_hint(it, sz, Chromosome(sz.width(), sz.height(), n, 0));
		it++->second.decode_scores(s);
	}
	assert(s.good());

	ibitstream bits(s);
	for(auto &[sz, c] : ret.chromosomes_)
		c.decode_patterns(bits);
	assert(s.good());

	return ret;
}

string Genome::sketch() const {
	stringstream o;
	string prefix;

	o << "{ ";
	for(auto [sz, c] : chromosomes_)
		if(c.size()) {
			o << prefix << sz << ": " << c.sketch() << endl;
			prefix = ", ";
		}
	o << "}";

	return o.str();
}

ostream &operator<<(ostream &o, const Genome &g) {
	o << "Genome {" << endl;
	for(const auto &[k, v] : g.chromosomes_)
		if(v.size()) o << "\t" << k << ": " << v << endl;
	return o << "}" << endl;
}

void save_population(ostream &file, const vector<Genome> &pop) {
	assert(pop.size() < 0x10000);
	file << uint8_t(pop.size() >> 8) << uint8_t(pop.size());
	for(const Genome &g : pop) g.encode(file);
}

vector<Genome> load_population(istream &file) {
	char byte;
	int sz;
	file.get(byte); sz = byte;
	file.get(byte); sz = (sz << 8) | byte;

	vector<Genome> ret; ret.reserve(sz);
	for(int i = 0; i < sz; ++i) ret.push_back(Genome::decode(file));

	return ret;
}

obitstream::~obitstream() {
	if(next_bit_ > 0) s_ << next_byte_;
}

obitstream &obitstream::operator<<(bool b) {
	next_byte_ |= (b << next_bit_++);
	if(next_bit_ >= 8) {
		s_ << next_byte_;
		next_byte_ = 0;
		next_bit_ = 0;
	}
	return *this;
}

bool ibitstream::get() {
	if(!next_bit_) s_.get(next_byte_);
	bool ret = next_byte_ & 1;
	next_byte_ >>= 1;
	if(++next_bit_ >= 8) next_bit_ = 0;
	return ret;
}

extern "C" {
	Boards *boards_new(char *base_board, char *diffs) { return new Boards(base_board, diffs); }
	void boards_delete(Boards *bs) { delete bs; }
	int boards_size(Boards *bs) { return bs->size(); }

	Chromosome *chromosome_new(int w, int h, int n, float p) { return new Chromosome(w, h, n, p); }
	Chromosome *chromosome_clone(Chromosome *g) { return new Chromosome(g->clone()); }
	void chromosome_delete(Chromosome *g) { delete g; }

	int chromosome_size(Chromosome *g) { return g->size(); }
	int chromosome_conv_width(Chromosome *g) { return g->conv_width(); }
	int chromosome_conv_height(Chromosome *g) { return g->conv_height(); }

	bool chromosome_get_color_pattern(Chromosome *g, int n, int c, int w, int h) { return g->get_color_pattern(n, c, w, h); }
	bool chromosome_get_shape_pattern(Chromosome *g, int n, int s, int w, int h) { return g->get_shape_pattern(n, s, w, h); }
	float chromosome_get_pattern_score(Chromosome *g, int n) { return g->get_pattern_score(n); }
	char *chromosome_encode_patterns(Chromosome *g, int *o_length);
	void patterns_encoding_delete(char *code) { delete code; }

	void chromosome_set_color_pattern(Chromosome *g, int n, int c, int w, int h, bool v) { return g->set_color_pattern(n, c, w, h, v); }
	void chromosome_set_shape_pattern(Chromosome *g, int n, int s, int w, int h, bool v) { return g->set_shape_pattern(n, s, w, h, v); }
	void chromosome_set_pattern_score(Chromosome *g, int n, float v) { return g->set_pattern_score(n, v); }
	void chromosome_decode_patterns(Chromosome *g, char *code, int length) { g->decode_patterns(string(code, length)); }

	Chromosome *chromosome_indices(Chromosome *g, int *is, int is_size);
	Chromosome *chromosome_append(Chromosome *g, Chromosome *other) { return new Chromosome(*g + *other); }

	void chromosome_dump(Chromosome *g) { cout << *g << endl; }
	void chromosome_sketch(Chromosome *g) { cout << g->sketch() << endl; }
	void chromosome_evaluate(Chromosome *g, Boards *bs, float *out);

	Genome *genome_new() { return new Genome(); }
	Genome *genome_clone(Genome *g) { return new Genome(g->clone()); }
	Genome *genome_from_chromosomes(Chromosome **cs, int size);
	void genome_delete(Genome *g) { delete g; }

	int genome_size(Genome *g) { return g->sizes().size(); }
	int genome_conv_width(Genome *g, int i) { return g->sizes()[i].width(); }
	int genome_conv_height(Genome *g, int i) { return g->sizes()[i].height(); }

	Chromosome *genome_get_chromosome(Genome *g, int w, int h) { return &g->get_chromosome(w, h); }
	void genome_set_chromosome(Genome *g, Chromosome *c) { g->set_chromosome(*c); }

	void genome_evaluate(Genome *g, Boards *bs, float *out);

	void genome_dump(Genome *g) { cout << *g << endl; }
	void genome_sketch(Genome *g) { cout << g->sketch() << endl; }

	void population_save(char *path, Genome **pop, int n);
	void population_load(char *path, Genome ***pop, int *n);
	void population_delete(Genome **pop) { delete[](pop); }
}

char *chromosome_encode_patterns(Chromosome *g, int *o_length) {
	string code = g->encode_patterns();
	*o_length = code.size();
	char *result = new char[code.size()];
	copy(code.begin(), code.end(), result);
	return result;
}

Chromosome *chromosome_indices(Chromosome *g, int *is, int is_size) {
	vector<int64_t> is_vec(is_size);
	for(int i = 0; i < is_size; ++i) is_vec[i] = is[i];
	return new Chromosome(g->indices(is_vec));
}

void chromosome_evaluate(Chromosome *g, Boards *bs, float *out) {
	Tensor out_tensor = g->evaluate(*bs).to(kCPU).contiguous();
	copy(out_tensor.data_ptr<float>(), out_tensor.data_ptr<float>() + bs->size(), out);
}

Genome *genome_from_chromosomes(Chromosome **cs, int size) {
	Genome *g = new Genome();
	for(int i = 0; i < size; ++i) *g += *cs[i];
	return g;
}

void genome_evaluate(Genome *g, Boards *bs, float *out) {
	Tensor out_tensor = g->evaluate(*bs).to(kCPU).contiguous();
	copy(out_tensor.data_ptr<float>(), out_tensor.data_ptr<float>() + bs->size(), out);
}

void population_save(char *path, Genome **pop, int n) {
	ofstream o(path, ofstream::out | ofstream::binary);
	vector<Genome> pop_vec; pop_vec.reserve(n);
	for(int i = 0; i < n; ++i) pop_vec.push_back(*pop[i]);
	save_population(o, pop_vec);
}

void population_load(char *path, Genome ***pop, int *n) {
	ifstream i(path, ifstream::in | ifstream::binary);
	vector<Genome> pop_vec = load_population(i);
	*n = pop_vec.size();
	*pop = new Genome *[pop_vec.size()];
	for(int i = 0; i < pop_vec.size(); ++i) (*pop)[i] = new Genome(pop_vec[i]);
}
