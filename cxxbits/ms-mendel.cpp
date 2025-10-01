#include <iostream>

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
const Tensor MIRROR = torch::tensor({0,1,3,2,4,5}); // swaps East (2) and West (3) shapes
const indexing::Slice ALL;

class Boards {
	public:
		// will read 128 bytes (16 8-byte chunks) from base_board:
		// 	* the empty cell is represented by the byte 0b00010011 (= 0x13 = SHAPES << 2 | COLORS)
		// 	* bottom two bits are color
		// 	* next two bits are shape
		// (we could in principle back two cells into each byte, but the extra
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
		Tensor p_color(int64_t conv_width, int64_t conv_height) const;
		Tensor p_shape(int64_t conv_width, int64_t conv_height) const;

		string sketch() const;
		friend ostream &operator<<(ostream &o, const Boards &g);

	protected:
		// TODO: could probably make this be (3w-2) x (3h-2) instead of 3w x 3h
		// num boards x ((COLORS or SHAPES) + SENTINELS) x (3*BOARD_WIDTH) x (3*BOARD_HEIGHT)
		Tensor color_, shape_;
};

class Genome {
	public:
		Genome(int64_t conv_width, int64_t conv_height, int64_t num_patterns, float p, bool mirroring);
		Genome(const Tensor &color_pattern, const Tensor &shape_pattern, const Tensor &pattern_score, bool mirroring);
		Genome clone() const;

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

		// Assuming all the scores are in the range (-1, 1), insert a little
		// noise, by adding a normal distribution with the given variance in
		// tanh space.
		//
		// Could be implemented in terms of get_score and set_score, but this
		// is faster, less annoying to implement, and less annoying to use.
		void tweak_pattern_scores(float variance);

		const Tensor &p_color_pattern() const;
		const Tensor &p_shape_pattern() const;
		const Tensor &p_pattern_score() const;

		Genome indices(vector<int64_t> is) const;
		Genome operator+(const Genome &other) const;

		string sketch() const;
		friend ostream &operator<<(ostream &o, const Genome &g);

		const bool mirroring_;

	protected:
		static void assert_compatible(const Tensor &t, const TensorOptions &o);

		int64_t mirroring_size() const { return mirroring_ + 1; }
		int64_t expansion_factor() const { return mirroring_size() * NUM_PERMUTATIONS; }
		int64_t p_size() const { return expansion_factor() * size(); }

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
		// These variants are 6x or 12x as long (in the first dimension) -- one
		// extra copy per color permutation per mirroring. They may be
		// undefined -- use the accessor functions with the similar name to
		// always get something defined.
		mutable Tensor p_color_pattern_, p_shape_pattern_, p_pattern_score_;
};

Tensor evaluate(const Genome &g, const Boards &bs);

Boards::Boards(char *base_board, char *diffs) {
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

	Tensor color = base_color.expand({num_boards, -1, -1, -1}).clone();
	Tensor shape = base_shape.expand({num_boards, -1, -1, -1}).clone();

	i = 0;
	while(*diffs != '\xfe') {
		if(*diffs == '\xff') {
			++i;
			++diffs;
			continue;
		}

		int x = MASK_AND_SHIFT(diffs, X), y = MASK_AND_SHIFT(diffs, Y);
		color.index_put_({i, ALL, x, y}, 0);
		shape.index_put_({i, ALL, x, y}, 0);
		++diffs;

		color[i][MASK_AND_SHIFT(diffs, Color)][x][y] = 1;
		shape[i][MASK_AND_SHIFT(diffs, Shape)][x][y] = 1;
		++diffs;
	}
#undef MASK_AND_SHIFT

	indexing::Slice board_x(BOARD_WIDTH, 2*BOARD_WIDTH), board_y(BOARD_HEIGHT, 2*BOARD_HEIGHT);
	color_ = torch::zeros({num_boards, COLORS+SENTINELS, 3*BOARD_WIDTH, 3*BOARD_HEIGHT}, GPU_BOOL_REP);
	shape_ = torch::zeros({num_boards, SHAPES+SENTINELS, 3*BOARD_WIDTH, 3*BOARD_HEIGHT}, GPU_BOOL_REP);
	color_.index_put_({ALL, COLOR_OUT_OF_BOUNDS, ALL, ALL}, 1);
	shape_.index_put_({ALL, SHAPE_OUT_OF_BOUNDS, ALL, ALL}, 1);
	color_.index_put_({ALL, ALL, board_x, board_y}, color);
	shape_.index_put_({ALL, ALL, board_x, board_y}, shape);
}

Tensor Boards::p_color(int64_t w, int64_t h) const {
	assert(0 < w && w <= BOARD_WIDTH && 0 < h && h <= BOARD_HEIGHT);
	--w;
	--h;
	return color_.index({ALL, ALL, indexing::Slice(BOARD_WIDTH-w, 2*BOARD_WIDTH+w), indexing::Slice(BOARD_HEIGHT-h, 2*BOARD_HEIGHT+h)});
}

Tensor Boards::p_shape(int64_t w, int64_t h) const {
	assert(0 < w && w <= BOARD_WIDTH && 0 < h && h <= BOARD_HEIGHT);
	--w;
	--h;
	return shape_.index({ALL, ALL, indexing::Slice(BOARD_WIDTH-w, 2*BOARD_WIDTH+w), indexing::Slice(BOARD_HEIGHT-h, 2*BOARD_HEIGHT+h)});
}

string Boards::sketch() const {
	stringstream o;
	o << "{ color: " << TensorSketch(color_) << ", shape: " << TensorSketch(shape_) << " }";
	return o.str();
}

ostream &operator<<(ostream &o, const Boards &bs) {
	string prefix;
	o << "Board { color = " << bs.color_ << ", shape = " << bs.shape_ << " }";
	return o;
}

Genome::Genome(int64_t w, int64_t h, int64_t n, float p, bool mirroring) : mirroring_(mirroring) {
	color_pattern_ = (torch::rand({n, COLORS + SENTINELS, w, h}, GPU_FLOAT) < p).to(GPU_BOOL_REP);
	shape_pattern_ = (torch::rand({n, SHAPES + SENTINELS, w, h}, GPU_FLOAT) < p).to(GPU_BOOL_REP);
	pattern_score_ = 2*torch::rand({n}, GPU_FLOAT) - 1;

	assert(!color_pattern_.requires_grad());
	assert(!shape_pattern_.requires_grad());
	assert(!pattern_score_.requires_grad());
}

Genome::Genome(const Tensor &co, const Tensor &sh, const Tensor &sc, bool mirroring)
	: color_pattern_(co), shape_pattern_(sh), pattern_score_(sc), mirroring_(mirroring)
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

	assert(!color_pattern_.requires_grad());
	assert(!shape_pattern_.requires_grad());
	assert(!pattern_score_.requires_grad());
}

Genome Genome::clone() const {
	Genome result(color_pattern_.clone(), shape_pattern_.clone(), pattern_score_.clone(), mirroring_);
	// we always set these fields back to Tensor() before modifying them, so no need to clone
	result.p_color_pattern_ = p_color_pattern_;
	result.p_shape_pattern_ = p_shape_pattern_;
	result.p_pattern_score_ = p_pattern_score_;
	return result;
}

bool Genome::get_color_pattern(int64_t pattern, int64_t color, int64_t w, int64_t h) const {
	return color_pattern_[pattern][color][w][h].item<CXX_BOOL_REP>() != 0;
}

bool Genome::get_shape_pattern(int64_t pattern, int64_t shape, int64_t w, int64_t h) const {
	return shape_pattern_[pattern][shape][w][h].item<CXX_BOOL_REP>() != 0;
}

float Genome::get_pattern_score(int64_t pattern) const {
	return pattern_score_[pattern].item<float>();
}

string Genome::encode_patterns() const {
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

	// this is sort of a hack, but: when being asked to save state, we're about
	// to potentially use this thing from many threads at once, so let's do the
	// thread unsafe bits here
	p_color_pattern();
	p_shape_pattern();

	return result;
}

void Genome::set_color_pattern(int64_t pattern, int64_t color, int64_t w, int64_t h, bool v) {
	color_pattern_[pattern][color][w][h] = v;
	p_color_pattern_ = Tensor();
}

void Genome::set_shape_pattern(int64_t pattern, int64_t shape, int64_t w, int64_t h, bool v) {
	shape_pattern_[pattern][shape][w][h] = v;
	p_shape_pattern_ = Tensor();
}

void Genome::set_pattern_score(int64_t pattern, float v) {
	pattern_score_[pattern] = v;
	p_pattern_score_ = Tensor();
}

void Genome::decode_patterns(string ps) {
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

void Genome::tweak_pattern_scores(float variance) {
	pattern_score_ = (pattern_score_.clamp(-0.9999999, 0.9999999).atanh() + variance * torch::randn({size()}, GPU_FLOAT)).tanh();
	p_pattern_score_ = Tensor();
}

const Tensor &Genome::p_color_pattern() const {
	if(!p_color_pattern_.defined()) {
		p_color_pattern_ = torch::zeros({mirroring_size(), NUM_PERMUTATIONS, size(), COLORS+SENTINELS, conv_width(), conv_height()}, GPU_BOOL_REP);
		for(int i = 0; i < NUM_PERMUTATIONS; ++i) {
			p_color_pattern_.index_put_
				( {0, i, "..."}
				, color_pattern_.index({ALL, PERMUTATIONS[i], "..."})
				);
			if(mirroring_) {
				p_color_pattern_.index_put_
					( {1, i, "..."}
					, p_color_pattern_.index({0, i, "..."}).flip(CONV_WIDTH_DIM)
					);
			}
		}
		p_color_pattern_ = p_color_pattern_.reshape({p_size(), COLORS+SENTINELS, conv_width(), conv_height()});
	}
	return p_color_pattern_;
}

const Tensor &Genome::p_shape_pattern() const {
	if(!p_shape_pattern_.defined()) {
		p_shape_pattern_ = shape_pattern_
			.expand({mirroring_size(), NUM_PERMUTATIONS, -1, -1, -1, -1});
		if(mirroring_) {
			// expand makes a compact view with many indices backed by the same
			// memory, but we're about to write in a way that shatters the
			// invariant needed for that to work, so we gotta clone
			p_shape_pattern_ = p_shape_pattern_.clone();
			p_shape_pattern_.index_put_
				( {1, "..."}
				, p_shape_pattern_.index({0, ALL, ALL, MIRROR, "..."}).flip(1+CONV_WIDTH_DIM)
				);
		}
		p_shape_pattern_ = p_shape_pattern_.reshape({p_size(), SHAPES+SENTINELS, conv_width(), conv_height()});
	}
	return p_shape_pattern_;
}

const Tensor &Genome::p_pattern_score() const {
	if(!p_pattern_score_.defined()) {
		p_pattern_score_ = pattern_score_
			.expand({expansion_factor(), size()})
			.reshape({p_size()});
	}
	return p_pattern_score_;
}

Genome Genome::indices(vector<int64_t> is) const {
	Tensor tis = torch::tensor(is);
	return Genome(color_pattern_.index({tis, "..."}), shape_pattern_.index({tis, "..."}), pattern_score_.index({tis}), mirroring_);
}

Genome Genome::operator+(const Genome &other) const {
	int64_t sz = size(), new_sz = size() + other.size(), w = conv_width(), h = conv_height();

	assert(other.conv_width() == w);
	assert(other.conv_height() == h);

	Tensor co, sh, sc;
	co = torch::zeros({new_sz, COLORS+SENTINELS, w, h}, GPU_BOOL_REP);
	sh = torch::zeros({new_sz, SHAPES+SENTINELS, w, h}, GPU_BOOL_REP);
	sc = torch::zeros({new_sz}, GPU_FLOAT);

	co.index_put_({indexing::Slice(0, sz), "..."}, color_pattern_);
	sh.index_put_({indexing::Slice(0, sz), "..."}, shape_pattern_);
	sc.index_put_({indexing::Slice(0, sz)}, pattern_score_);

	co.index_put_({indexing::Slice(sz), "..."}, other.color_pattern_);
	sh.index_put_({indexing::Slice(sz), "..."}, other.shape_pattern_);
	sc.index_put_({indexing::Slice(sz)}, other.pattern_score_);

	return Genome(co, sh, sc, mirroring_ || other.mirroring_);
}

string Genome::sketch() const {
	stringstream o;

	o << "{ color: " << TensorSketch(color_pattern_);
	o << ", shape: " << TensorSketch(shape_pattern_);
	o << ", score: " << TensorSketch(pattern_score_);
	if(p_color_pattern_.defined()) o << ", color cache: " << TensorSketch(p_color_pattern_);
	if(p_shape_pattern_.defined()) o << ", shape cache: " << TensorSketch(p_shape_pattern_);
	if(p_pattern_score_.defined()) o << ", score cache: " << TensorSketch(p_pattern_score_);
	o << ", " << (mirroring_ ? "" : "no ") << "mirroring";
	o << " }";

	return o.str();
}

ostream &operator<<(ostream &o, const Genome &g) {
	string prefix;
	o << "Genome {size = " << g.size() << ", mirroring = " << (g.mirroring_ ? "true" : "false") << ", permutation cache = {";
	if(g.p_color_pattern_.defined()) { o << prefix << "color = " << g.p_color_pattern_; prefix = ", "; }
	if(g.p_shape_pattern_.defined()) { o << prefix << "shape = " << g.p_shape_pattern_; prefix = ", "; }
	if(g.p_pattern_score_.defined()) { o << prefix << "score = " << g.p_pattern_score_; prefix = ", "; }
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

void Genome::assert_compatible(const Tensor &t, const TensorOptions &o) {
	assert(t.dtype() == o.dtype());
	assert(t.device().type() == o.device().type());
}

Tensor evaluate(const Genome &g, const Boards &bs) {
	if(g.size() == 0) return torch::zeros({bs.size()}, CPU_FLOAT /* we're about to move it to the CPU anyway */);
	const int64_t cw = g.conv_width(), ch = g.conv_height();
	Tensor mismatch_color = conv2d(bs.p_color(cw, ch), g.p_color_pattern()),
	       mismatch_shape = conv2d(bs.p_shape(cw, ch), g.p_shape_pattern());
	Tensor match = ((mismatch_color + mismatch_shape) == 0).to(GPU_BYTE);
	return (match.sum({2,3})*g.p_pattern_score()).sum({1});
}

extern "C" {
	Boards *boards_new(char *base_board, char *diffs) { return new Boards(base_board, diffs); }
	void boards_delete(Boards *bs) { delete bs; }
	int boards_size(Boards *bs) { return bs->size(); }

	Genome *genome_new(int w, int h, int n, float p, bool mirroring) { return new Genome(w, h, n, p, mirroring); }
	Genome *genome_clone(Genome *g) { return new Genome(g->clone()); }
	void genome_delete(Genome *g) { delete g; }

	int genome_size(Genome *g) { return g->size(); }
	int genome_conv_width(Genome *g) { return g->conv_width(); }
	int genome_conv_height(Genome *g) { return g->conv_height(); }
	bool genome_mirroring(Genome *g) { return g->mirroring_; }

	bool genome_get_color_pattern(Genome *g, int n, int c, int w, int h) { return g->get_color_pattern(n, c, w, h); }
	bool genome_get_shape_pattern(Genome *g, int n, int s, int w, int h) { return g->get_shape_pattern(n, s, w, h); }
	float genome_get_pattern_score(Genome *g, int n) { return g->get_pattern_score(n); }
	char *genome_encode_patterns(Genome *g, int *o_length);
	void patterns_encoding_delete(char *code) { delete[] code; }

	void genome_set_color_pattern(Genome *g, int n, int c, int w, int h, bool v) { return g->set_color_pattern(n, c, w, h, v); }
	void genome_set_shape_pattern(Genome *g, int n, int s, int w, int h, bool v) { return g->set_shape_pattern(n, s, w, h, v); }
	void genome_set_pattern_score(Genome *g, int n, float v) { return g->set_pattern_score(n, v); }
	void genome_decode_patterns(Genome *g, char *code, int length) { g->decode_patterns(string(code, length)); }

	void genome_tweak_pattern_scores(Genome *g, float variance) { g->tweak_pattern_scores(variance); }

	Genome *genome_indices(Genome *g, int *is, int is_size);
	Genome *genome_append(Genome *g, Genome *other) { return new Genome(*g + *other); }

	void genome_dump(Genome *g) { cout << *g << endl; }
	void genome_sketch(Genome *g) { cout << g->sketch() << endl; }

	void evaluate(Genome *g, Boards *bs, float *out);
}

char *genome_encode_patterns(Genome *g, int *o_length) {
	string code = g->encode_patterns();
	*o_length = code.size();
	char *result = new char[code.size()];
	copy(code.begin(), code.end(), result);
	return result;
}

Genome *genome_indices(Genome *g, int *is, int is_size) {
	vector<int64_t> is_vec(is_size);
	for(int i = 0; i < is_size; ++i) is_vec[i] = is[i];
	return new Genome(g->indices(is_vec));
}

void evaluate(Genome *g, Boards *bs, float *out) {
	Tensor out_tensor = evaluate(*g, *bs).to(kCPU).contiguous();
	copy(out_tensor.data_ptr<float>(), out_tensor.data_ptr<float>() + bs->size(), out);
}
