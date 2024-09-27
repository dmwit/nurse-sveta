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

const Tensor PERMUTATIONS = torch::tensor({0,1,2,3,4,0,2,1,3,4,1,0,2,3,4,1,2,0,3,4,2,0,1,3,4,2,1,0,3,4}).reshape({NUM_PERMUTATIONS,COLORS+SENTINELS});

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
		const Tensor &p_color(int64_t conv_width, int64_t conv_height) const;
		const Tensor &p_shape(int64_t conv_width, int64_t conv_height) const;

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

class Genome {
	public:
		Genome(int64_t conv_width, int64_t conv_height, int64_t num_patterns = 0, float p = 0.5);
		Genome(const Tensor &color_pattern, const Tensor &shape_pattern, const Tensor &pattern_score);

		int64_t size() const { return color_pattern_.size(INDEX_DIM); }
		int64_t conv_width() const { return color_pattern_.size(CONV_WIDTH_DIM); }
		int64_t conv_height() const { return color_pattern_.size(CONV_HEIGHT_DIM); }

		const Tensor &p_color_pattern() const;
		const Tensor &p_shape_pattern() const;
		const Tensor &p_pattern_score() const;

		Genome indices(vector<int64_t> is) const;
		Genome operator+(const Genome &other) const;

		string sketch() const;
		friend ostream &operator<<(ostream &o, const Genome &g);

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

const Tensor &Boards::p_color(int64_t w, int64_t h) const {
	if(!p_color_.defined() || p_conv_width_ != w || p_conv_height_ != h) {
		// we sort of assume that if you're asking for p_color(), you're about
		// to ask for p_shape(), and generate 'em both
		generate_padding_cache(w, h);
	}
	return p_color_;
}

const Tensor &Boards::p_shape(int64_t w, int64_t h) const {
	if(!p_shape_.defined() || p_conv_width_ != w || p_conv_height_ != h) {
		// we sort of assume that if you're asking for p_shape(), you're about
		// to ask for p_color(), and generate 'em both
		generate_padding_cache(w, h);
	}
	return p_shape_;
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

Genome::Genome(int64_t w, int64_t h, int64_t n, float p) {
	color_pattern_ = (torch::rand({n, COLORS + SENTINELS, w, h}, GPU_FLOAT) < p).to(GPU_BOOL_REP);
	shape_pattern_ = (torch::rand({n, SHAPES + SENTINELS, w, h}, GPU_FLOAT) < p).to(GPU_BOOL_REP);
	pattern_score_ = torch::randn({n}, GPU_FLOAT);
	normalize_scores();

	assert(!color_pattern_.requires_grad());
	assert(!shape_pattern_.requires_grad());
	assert(!pattern_score_.requires_grad());
}

Genome::Genome(const Tensor &co, const Tensor &sh, const Tensor &sc)
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

const Tensor &Genome::p_color_pattern() const {
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

const Tensor &Genome::p_shape_pattern() const {
	if(!p_shape_pattern_.defined()) {
		p_shape_pattern_ = shape_pattern_
			.expand({NUM_PERMUTATIONS, -1, -1, -1, -1})
			.reshape({NUM_PERMUTATIONS*size(), SHAPES+SENTINELS, conv_width(), conv_height()});
	}
	return p_shape_pattern_;
}

const Tensor &Genome::p_pattern_score() const {
	if(!p_pattern_score_.defined()) {
		p_pattern_score_ = pattern_score_
			.expand({NUM_PERMUTATIONS, size()})
			.reshape({NUM_PERMUTATIONS*size()});
	}
	return p_pattern_score_;
}

Genome Genome::indices(vector<int64_t> is) const {
	Tensor tis = torch::tensor(is);
	return Genome(color_pattern_.index({tis, "..."}), shape_pattern_.index({tis, "..."}), pattern_score_.index({tis}));
}

Genome Genome::operator+(const Genome &other) const {
	int64_t sz = size(), new_sz = size() + other.size(), w = conv_width(), h = conv_height();

	assert(other.conv_width() == w);
	assert(other.conv_height() == w);

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

	return Genome(co, sh, sc);
}

string Genome::sketch() const {
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

ostream &operator<<(ostream &o, const Genome &g) {
	string prefix;
	o << "Genome {size = " << g.size() << ", permutation cache = {";
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

void Genome::normalize_scores() {
	if(size() <= 0) return;
	pattern_score_ /= pattern_score_.abs().max();
	p_pattern_score_ = Tensor();
}

void Genome::assert_compatible(const Tensor &t, const TensorOptions &o) {
	assert(t.dtype() == o.dtype());
	assert(t.device().type() == o.device().type());
}

Tensor evaluate(const Genome &g, const Boards &bs) {
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

	Genome *genome_new(int w, int h, int n, float p) { return new Genome(w, h, n, p); }
	void genome_delete(Genome *g) { delete g; }
	int genome_size(Genome *g) { return g->size(); }
	int genome_conv_width(Genome *g) { return g->conv_width(); }
	int genome_conv_height(Genome *g) { return g->conv_height(); }
	Genome *genome_indices(Genome *g, int *is, int is_size);
	Genome *genome_append(Genome *g, Genome *other) { return new Genome(*g + *other); }
	void genome_dump(Genome *g) { cout << *g << endl; }
	void genome_sketch(Genome *g) { cout << g->sketch() << endl; }

	void evaluate(Genome *g, Boards *bs, float *out);
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
