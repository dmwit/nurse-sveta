#include <iostream>

#include "constants.hpp"
#include "debugging.hpp"

using namespace std;
using namespace torch;

constexpr int64_t SENTINELS = 2; // empty, out-of-bounds
constexpr int64_t NUM_COLORINGS = 6, NUM_MIRRORINGS = 2;
constexpr int64_t INDEX_DIM = 0, ONEHOT_DIM = 1, CONV_WIDTH_DIM = 2, CONV_HEIGHT_DIM = 3;
constexpr int64_t MIRRORING_DIM = 0, COLORING_DIM = 1, EXPANSION_DIMS = 2;
const int64_t COLOR_OUT_OF_BOUNDS = COLORS + 1, SHAPE_OUT_OF_BOUNDS = SHAPES + 1;

constexpr int64_t EINDEX_DIM = EXPANSION_DIMS+INDEX_DIM,
                  EONEHOT_DIM = EXPANSION_DIMS+ONEHOT_DIM,
                  ECONV_WIDTH_DIM = EXPANSION_DIMS+CONV_WIDTH_DIM,
                  ECONV_HEIGHT_DIM = EXPANSION_DIMS+CONV_HEIGHT_DIM;

// ideally we'd use kBool, but convolutions aren't implemented for those
// see also https://github.com/pytorch/pytorch/issues/136578
#define GPU_BOOL_REP GPU_HALF
typedef float CXX_BOOL_REP;

const Tensor PERMUTATIONS = torch::tensor({0,1,2,3,4,0,2,1,3,4,1,0,2,3,4,1,2,0,3,4,2,0,1,3,4,2,1,0,3,4}).reshape({NUM_COLORINGS,COLORS+SENTINELS});
const Tensor MIRROR = torch::tensor({0,1,3,2,4,5}); // swaps East (2) and West (3) shapes
const indexing::Slice ALL;

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
		Tensor p_color(int64_t conv_width, int64_t conv_height) const;
		Tensor p_shape(int64_t conv_width, int64_t conv_height) const;

		string sketch() const;
		friend ostream &operator<<(ostream &o, const Boards &g);

	protected:
		// TODO: could probably make this be (3w-2) x (3h-2) instead of 3w x 3h
		// num boards x ((COLORS or SHAPES) + SENTINELS) x (3*BOARD_WIDTH) x (3*BOARD_HEIGHT)
		Tensor color_, shape_;
};

class Patterns;
class PatternsTemplate {
	public:
		PatternsTemplate(int64_t conv_width, int64_t conv_height, int64_t num_patterns);
		PatternsTemplate(const string &encoding);

		int64_t size() const { return color_pattern_.size(INDEX_DIM); }
		int64_t conv_width() const { return color_pattern_.size(CONV_WIDTH_DIM); }
		int64_t conv_height() const { return color_pattern_.size(CONV_HEIGHT_DIM); }

		bool get_color_pattern(int64_t pattern, int64_t color, int64_t x, int64_t y) const;
		bool get_shape_pattern(int64_t pattern, int64_t shape, int64_t x, int64_t y) const;

		void set_color_pattern(int64_t pattern, int64_t color, int64_t x, int64_t y, bool v);
		void set_shape_pattern(int64_t pattern, int64_t color, int64_t x, int64_t y, bool v);

		string encode() const;
		string sketch() const;
		friend ostream &operator<<(ostream &o, const PatternsTemplate &t);
		friend Patterns;

	protected:
		void reset_patterns(int64_t conv_width, int64_t conv_height, int64_t num_patterns);

		// patterns have a 1 where that color/shape is forbidden and a 0 where
		// it's allowed (chosen this way so that the convolution operator is
		// exactly the right operation for telling whether the pattern matches
		// at each position)
		// num patterns x (COLORS + SENTINELS) x conv width x conv height @ GPU_BOOL_REP
		Tensor color_pattern_;
		// num patterns x (SHAPES + SENTINELS) x conv width x conv height @ GPU_BOOL_REP
		Tensor shape_pattern_;
};

class Patterns {
	public:
		Patterns(const PatternsTemplate &t, bool mirroring, bool coloring);
		string sketch() const;

		int64_t size() const { return color_pattern_.size(EINDEX_DIM); }
		int64_t conv_width() const { return color_pattern_.size(ECONV_WIDTH_DIM); }
		int64_t conv_height() const { return color_pattern_.size(ECONV_HEIGHT_DIM); }
		int64_t mirroring_size() const { return color_pattern_.size(MIRRORING_DIM); }
		int64_t coloring_size() const { return color_pattern_.size(COLORING_DIM); }
		int64_t replication_size() const { return mirroring_size() * coloring_size(); }

		// {mirroring?2:1} x {coloring?6:1} x num patterns x (COLORS + SENTINELS) x conv width x conv height @GPU_BOOL_REP
		const Tensor color_pattern_;
		// {mirroring?2:1} x {coloring?6:1} x num patterns x (SHAPES + SENTINELS) x conv width x conv height @GPU_BOOL_REP
		const Tensor shape_pattern_;

	protected:
		static Tensor expand_colors(const Tensor &t, bool mirroring, bool coloring);
		static Tensor expand_shapes(const Tensor &t, bool mirroring, bool coloring);
};

// returned tensor is
//     bs.size() x ps.replication_size() x ps.size() x
//     (BOARD_WIDTH + ps.conv_width() - 1) x (BOARD_HEIGHT + ps.conv_height() - 1)
//     @GPU_BOOL_REP
Tensor match_full(const Boards &bs, const Patterns &ps);
// argument is num_boards x r x num_patterns x w x h @GPU_BOOL_REP
// result is num_boards x num_patterns @GPU_I64
Tensor summarize_match(const Tensor &match);
// num_boards x num_patterns @GPU_I64 ->
// num_patterns @GPU_FLOAT ->
// num_boards @GPU_FLOAT
Tensor score(const Tensor &match_counts, const Tensor &scores);

// summarize_match . match_full
Tensor match_summary(const Boards &bs, const Patterns &ps);
// score . match_summary (= score . summarize_match . match_full)
Tensor evaluate(const Boards &bs, const Patterns &ps, const Tensor &scores);

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

PatternsTemplate::PatternsTemplate(int64_t w, int64_t h, int64_t n) {
	reset_patterns(w, h, n);
}

void PatternsTemplate::reset_patterns(int64_t w, int64_t h, int64_t n) {
	color_pattern_ = torch::ones({n, COLORS + SENTINELS, w, h}, GPU_BOOL_REP);
	shape_pattern_ = torch::ones({n, SHAPES + SENTINELS, w, h}, GPU_BOOL_REP);

	assert(!color_pattern_.requires_grad());
	assert(!shape_pattern_.requires_grad());
}

bool PatternsTemplate::get_color_pattern(int64_t pattern, int64_t color, int64_t x, int64_t y) const {
	return color_pattern_[pattern][color][x][y].item<CXX_BOOL_REP>() != 0;
}

bool PatternsTemplate::get_shape_pattern(int64_t pattern, int64_t shape, int64_t x, int64_t y) const {
	return shape_pattern_[pattern][shape][x][y].item<CXX_BOOL_REP>() != 0;
}

string PatternsTemplate::encode() const {
	const int64_t n = size(), w = conv_width(), h = conv_height();
	string result(2 + (n*(COLORS + SENTINELS + SHAPES + SENTINELS)*w*h+7)/8, '\0');
	int bit = 0, byte = 0;

	// metadata
	assert(0 <= n && n < 256);
	assert(1 <= h && h < 16);
	assert(1 <= w && w < 16);
	result[byte++] = n;
	result[byte  ] |= (h-1);
	result[byte++] |= (w-1) << 4;

	// data
	for(int pattern = 0; pattern < n; ++pattern) {
		for(int x = 0; x < w; ++x) {
			for(int y = 0; y < h; ++y) {
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

void PatternsTemplate::set_color_pattern(int64_t pattern, int64_t color, int64_t x, int64_t y, bool v) {
	color_pattern_[pattern][color][x][y] = v;
}

void PatternsTemplate::set_shape_pattern(int64_t pattern, int64_t shape, int64_t x, int64_t y, bool v) {
	shape_pattern_[pattern][shape][x][y] = v;
}

PatternsTemplate::PatternsTemplate(const string &ps) {
	int bit = 0, byte = 0;

	// metadata
	const int64_t n = byte < ps.size() ? ps[byte] : 0;
	++byte;
	const int64_t h = 1 + (byte < ps.size() ? (ps[byte] & 0xf) : 0),
	              w = 1 + (byte < ps.size() ? (ps[byte] >> 4) : 0);
	++byte;
	reset_patterns(w, h, n);

	// data
	for(int pattern = 0; pattern < n; ++pattern) {
		for(int x = 0; x < w; ++x) {
			for(int y = 0; y < h; ++y) {
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
}

string PatternsTemplate::sketch() const {
	stringstream o;

	o << "PatternsTemplate { color: " << TensorSketch(color_pattern_);
	o << ", shape: " << TensorSketch(shape_pattern_);
	o << " }";

	return o.str();
}

ostream &operator<<(ostream &o, const PatternsTemplate &t) {
	string prefix;
	o << "PatternsTemplate {size = " << t.size();

	prefix = "";
	for(int i = 0; i < t.size(); ++i) {
		o << ",\npattern " << i << " = { ";
		o << "color = " << t.color_pattern_[i] << "," << endl;
		o << "shape = " << t.shape_pattern_[i] << endl << "}";
		prefix = "\n";
	}
	o << prefix << "}";
	return o;
}

Patterns::Patterns(const PatternsTemplate &t, bool mirroring, bool coloring)
	: color_pattern_(expand_colors(t.color_pattern_, mirroring, coloring))
	, shape_pattern_(expand_shapes(t.shape_pattern_, mirroring, coloring))
	{}

Tensor Patterns::expand_colors(const Tensor &t, bool mirroring, bool coloring) {
	const int num_mirrorings = mirroring?NUM_MIRRORINGS:1;
	const int num_colorings = coloring?NUM_COLORINGS:1;
	Tensor expanded = torch::zeros({num_mirrorings, num_colorings, t.size(INDEX_DIM), COLORS+SENTINELS, t.size(CONV_WIDTH_DIM), t.size(CONV_HEIGHT_DIM)}, GPU_BOOL_REP);
	for(int i = 0; i < num_colorings; ++i) {
		expanded.index_put_({0, i, "..."}, t.index({ALL, PERMUTATIONS[i], "..."}));
		if(mirroring)
			expanded.index_put_({1, i, "..."}, expanded.index({0, i, "..."}).flip(CONV_WIDTH_DIM));
	}
	return expanded;
}

Tensor Patterns::expand_shapes(const Tensor &t, bool mirroring, bool coloring) {
	const int num_mirrorings = mirroring?NUM_MIRRORINGS:1;
	const int num_colorings = coloring?NUM_COLORINGS:1;
	Tensor expanded = t.expand({num_mirrorings, num_colorings, -1, -1, -1, -1});
	if(mirroring) {
		// expand makes a compact view with many indices backed by the same
		// memory, but we're about to write in a way that shatters the
		// invariant needed for that to work, so we gotta clone
		expanded = expanded.clone();
		expanded.index_put_({1, "..."}, expanded.index({0, ALL, ALL, MIRROR, "..."}).flip(ECONV_WIDTH_DIM));
	}
	return expanded;
}

string Patterns::sketch() const {
	stringstream o;

	o << "Patterns { color: " << TensorSketch(color_pattern_);
	o << ", shape: " << TensorSketch(shape_pattern_);
	o << " }";

	return o.str();
}

ostream &operator<<(ostream &o, const Patterns &ps) {
	string prefix;
	o << "Patterns {size = " << ps.size();

	prefix = "";
	for(int i = 0; i < ps.size(); ++i) {
		o << ",\npattern " << i << " = { ";
		o << "color = " << ps.color_pattern_.index({ALL, ALL, i, "..."}) << "," << endl;
		o << "shape = " << ps.shape_pattern_.index({ALL, ALL, i, "..."}) << endl << "}";
		prefix = "\n";
	}
	o << prefix << "}";
	return o;
}

Tensor match_full(const Boards &bs, const Patterns &ps) {
	if(ps.size() == 0) return torch::zeros(
		{bs.size(), ps.replication_size(), ps.size(), BOARD_WIDTH + ps.conv_width() - 1, BOARD_HEIGHT + ps.conv_height() - 1},
		GPU_BOOL_REP);
	const int64_t cw = ps.conv_width(), ch = ps.conv_height();
	Tensor color_pattern = ps.color_pattern_.view({-1, COLORS+SENTINELS, cw, ch}),
	       shape_pattern = ps.shape_pattern_.view({-1, SHAPES+SENTINELS, cw, ch});
	Tensor mismatch_color = conv2d(bs.p_color(cw, ch), color_pattern),
	       mismatch_shape = conv2d(bs.p_shape(cw, ch), shape_pattern);
	return ((mismatch_color + mismatch_shape) == 0).view({bs.size(), ps.replication_size(), ps.size(), mismatch_color.size(2), mismatch_color.size(3)});
}

Tensor summarize_match(const Tensor &match) {
	return match.sum({1,3,4});
}

Tensor score(const Tensor &match_counts, const Tensor &scores) {
	return (match_counts * scores).sum({1});
}

Tensor match_summary(const Boards &bs, const Patterns &ps) {
	return summarize_match(match_full(bs, ps));
}

Tensor evaluate(const Boards &bs, const Patterns &ps, const Tensor &scores) {
	return score(match_summary(bs, ps), scores);
}

Tensor to_cpu(const Tensor &t, int64_t n) {
	int64_t size = 1;
	for(int i = 0; i < t.dim(); ++i) {
		size *= t.size(i);
	}
	if(size != n) {
		cerr << "to_cpu(" << TensorSketch(t) << ", " << n << "): actual size does not match declared size" << endl;
		assert(false);
	}

	return t.to(kCPU).contiguous();
}

void to_cpu(const Tensor &t, float *out_pointer, int64_t n) {
	Tensor out_tensor = to_cpu(t, n);
	copy(out_tensor.data_ptr<float>(), out_tensor.data_ptr<float>() + n, out_pointer);
}

void to_cpu(const Tensor &t, int64_t *out_pointer, int64_t n) {
	Tensor out_tensor = to_cpu(t, n);
	copy(out_tensor.data_ptr<int64_t>(), out_tensor.data_ptr<int64_t>() + n, out_pointer);
}

extern "C" {
	Boards *boards_new(char *base_board, char *diffs) { return new Boards(base_board, diffs); }
	void boards_delete(Boards *bs) { delete bs; }
	int64_t boards_size(Boards *bs) { return bs->size(); }

	PatternsTemplate *patterns_template_new(int64_t w, int64_t h, int64_t n) { return new PatternsTemplate(w, h, n); }
	void patterns_template_delete(PatternsTemplate *t) { delete t; }

	int64_t patterns_template_conv_width(PatternsTemplate *t) { return t->conv_width(); }
	int64_t patterns_template_conv_height(PatternsTemplate *t) { return t->conv_height(); }
	int64_t patterns_template_size(PatternsTemplate *t) { return t->size(); }

	uint8_t patterns_template_get_color_pattern(PatternsTemplate *t, int64_t pattern, int64_t color, int64_t x, int64_t y) { return t->get_color_pattern(pattern, color, x, y); }
	uint8_t patterns_template_get_shape_pattern(PatternsTemplate *t, int64_t pattern, int64_t shape, int64_t x, int64_t y) { return t->get_shape_pattern(pattern, shape, x, y); }
	void patterns_template_set_color_pattern(PatternsTemplate *t, int64_t pattern, int64_t color, int64_t x, int64_t y, uint8_t v) { t->set_color_pattern(pattern, color, x, y, v); }
	void patterns_template_set_shape_pattern(PatternsTemplate *t, int64_t pattern, int64_t shape, int64_t x, int64_t y, uint8_t v) { t->set_shape_pattern(pattern, shape, x, y, v); }

	PatternsTemplate *patterns_template_decode(char *code, int64_t length) { return new PatternsTemplate(string(code, length)); }
	char *patterns_template_encode(PatternsTemplate *t, int64_t *o_length);
	void patterns_template_encoding_delete(char *code) { delete[] code; }

	Patterns *patterns_new(PatternsTemplate *t, uint8_t mirroring, uint8_t coloring) { return new Patterns(*t, mirroring, coloring); }
	void patterns_delete(Patterns *ps) { delete ps; }

	int64_t patterns_size(const Patterns *ps) { return ps->size(); }
	int64_t patterns_conv_width(const Patterns *ps) { return ps->conv_width(); }
	int64_t patterns_conv_height(const Patterns *ps) { return ps->conv_height(); }
	int64_t patterns_mirroring_size(const Patterns *ps) { return ps->mirroring_size(); }
	int64_t patterns_coloring_size(const Patterns *ps) { return ps->coloring_size(); }
	int64_t patterns_replication_size(const Patterns *ps) { return ps->replication_size(); }

	void tensor_delete(Tensor *t) { delete t; }
	void float_tensor_to_cpu(const Tensor *t, float *out, int64_t n) { to_cpu(*t, out, n); }
	void int_tensor_to_cpu(const Tensor *t, int64_t *out, int64_t n) { to_cpu(*t, out, n); }
	Tensor *tensor_clone(const Tensor *t) { return new Tensor(t->detach().clone()); }

	Tensor *match_full(const Boards *bs, const Patterns *ps) { return new Tensor(match_full(*bs, *ps)); }
	Tensor *summarize_match(const Tensor *match) { return new Tensor(summarize_match(*match)); }
	Tensor *score(const Tensor *match_counts, const Tensor *scores) { return new Tensor(score(*match_counts, *scores)); }
	Tensor *match_summary(const Boards *bs, const Patterns *ps) { return new Tensor(match_summary(*bs, *ps)); }
	Tensor *evaluate(const Boards *bs, const Patterns *ps, const Tensor *scores) { return new Tensor(evaluate(*bs, *ps, *scores)); }
	void evaluate_sync(const Boards *bs, const Patterns *ps, const Tensor *scores, float *out, int64_t n) { to_cpu(evaluate(*bs, *ps, *scores), out, n); }

	Tensor *scores_new(float *data, int64_t len) { return new Tensor(torch::from_blob(data, {len}).to(GPU_FLOAT)); }
	float scores_get(Tensor *t, int64_t i) { return (*t)[i].item<float>(); }
	void scores_set(Tensor *t, int64_t i, float v) { (*t)[i] = v; }

	Tensor *tensor_tanh(const Tensor *t) { return new Tensor(t->tanh()); }
	Tensor *tensor_add(const Tensor *l, const Tensor *r) { return new Tensor(*l + *r); }
	Tensor *tensor_scale(float a, const Tensor *t) { return new Tensor(a * *t); }

	void boards_dump(Boards *bs) { cout << *bs << endl; }
	void boards_sketch(Boards *bs) { cout << bs->sketch() << endl; }
	void patterns_template_dump(PatternsTemplate *t) { cout << *t << endl; }
	void patterns_template_sketch(PatternsTemplate *t) { cout << t->sketch() << endl; }
	void patterns_dump(Patterns *ps) { cout << *ps << endl; }
	void patterns_sketch(Patterns *ps) { cout << ps->sketch() << endl; }
	void tensor_dump(Tensor *t) { cout << *t << endl; }
	void tensor_sketch(Tensor *t) { cout << TensorSketch(*t) << endl; }
}

char *patterns_template_encode(PatternsTemplate *t, int64_t *o_length) {
	string code = t->encode();
	*o_length = code.size();
	char *result = new char[code.size()];
	copy(code.begin(), code.end(), result);
	return result;
}
