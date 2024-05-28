#include "constants.hpp"
#include "debugging.hpp"
#include "endpoint.hpp"

const int64_t LOOKAHEAD_SIZE = COLORS + COLORS;
const int64_t CELL_SIZE = COLORS + SHAPES;
const int64_t SCALARS = 6; // frames, log(frames), sqrt(frames), starting viruses, log(starting viruses), 1/sqrt(starting viruses)

const int64_t CELLS = BOARD_WIDTH * BOARD_HEIGHT;
const int64_t PILLCONTENTS = ORIENTATIONS*COLORS*COLORS;

// priors, occupied, virus kills, future pill placements, clear locations, valuation+fall time+clear pills
const int64_t OUTPUT_LAYERS = ROTATIONS + 1 + 1 + PILLCONTENTS + 1 + 1;
const int64_t OUTPUT_TYPES = 8;
const int64_t OUTPUT_SCALARS = PILLCONTENTS+2; // clear pills, valuation, fall time

const int64_t FILTERS = 64;
const int64_t RESIDUAL_BLOCKS = 20;
const float LEAKAGE = 0.01;

const int64_t BODY_SIZE = FILTERS * CELLS;

// TODO: A0 started its LR off at 0.2 (!)
const float INITIAL_LEARNING_RATE = 1e-3;
const float EPSILON = 1e-7;

// TODO: turn off for speed when we're fairly confident things are working right
constexpr bool CHECK_MASK_VALIDITY = true;
constexpr bool CHECK_NO_CLAMPING = true;

template<typename T> sp_endpoint generic_module_weights(T m);

struct NetInput {
	// boards: [n, 7, 8, 16]
	// lookaheads: [n, 6]
	// scalars: [n, 4]
	torch::Tensor boards, lookaheads, scalars;

	NetInput() { DebugScope dbg("NetInput()", DEFAULT_CONSTRUCTOR_VERBOSITY); }

	NetInput(int n, char *boards_data, char *lookaheads_data, float *scalars_data) {
		DebugScope dbg("NetInput(n, boards, lookaheads, scalars)", DEFAULT_CONSTRUCTOR_VERBOSITY);
		boards     = torch::from_blob(    boards_data, {n, CELL_SIZE, BOARD_WIDTH, BOARD_HEIGHT}, [](void *v){}, CPU_BYTE );
		lookaheads = torch::from_blob(lookaheads_data, {n, LOOKAHEAD_SIZE}                      , [](void *v){}, CPU_BYTE );
		scalars    = torch::from_blob(   scalars_data, {n, SCALARS}                             , [](void *v){}, CPU_FLOAT);
	}

	NetInput(int n) {
		DebugScope dbg("NetInput(n)", DEFAULT_CONSTRUCTOR_VERBOSITY);
		boards     = torch::empty({n, CELL_SIZE, BOARD_WIDTH, BOARD_HEIGHT}, CPU_BYTE );
		lookaheads = torch::empty({n, LOOKAHEAD_SIZE}                      , CPU_BYTE );
		scalars    = torch::empty({n, SCALARS}                             , CPU_FLOAT);
	}

	void to_gpu() {
		DebugScope dbg("NetInput::to_gpu", DEFAULT_TRANSFER_VERBOSITY);
		boards     = boards    .to(torch::kCUDA);
		lookaheads = lookaheads.to(torch::kCUDA);
		scalars    = scalars   .to(torch::kCUDA);
		boards     = boards    .to(torch::kF32);
		lookaheads = lookaheads.to(torch::kF32);
	}

	void save(torch::serialize::OutputArchive &archive) const {
		DebugScope dbg("NetInput::save", DEFAULT_SERIALIZATION_VERBOSITY);
		archive.write("boards"    , boards    , true);
		archive.write("lookaheads", lookaheads, true);
		archive.write("scalars"   , scalars   , true);
	}

	void load(torch::serialize::InputArchive &archive) {
		DebugScope dbg("NetInput::load", DEFAULT_SERIALIZATION_VERBOSITY);
		archive.read("boards"    , boards    , true);
		archive.read("lookaheads", lookaheads, true);
		archive.read("scalars"   , scalars   , true);
	}

	std::ostream &dump(std::ostream &o) const {
		DebugScope dbg("NetInput::dump", DEFAULT_SERIALIZATION_VERBOSITY);
		return o
			<<     "boards: " << boards     << std::endl
			<< "lookaheads: " << lookaheads << std::endl
			<<    "scalars: " << scalars    << std::endl
			;
	}

	std::ostream &sketch(std::ostream &o) const {
		DebugScope dbg("NetInput::sketch", DEFAULT_SERIALIZATION_VERBOSITY);
		return o
			<<     "boards: " << TensorSketch(boards    ) << ", "
			<< "lookaheads: " << TensorSketch(lookaheads) << ", "
			<<    "scalars: " << TensorSketch(scalars   )
			;
	}
};

std::ostream &operator<<(std::ostream &o, const NetInput &in) { return in.dump(o); }

struct NetOutput {
	// priors: [n, 4, 8, 16]
	// valuation: [n]
	// fall_time: [n]
	// occupied: [n, 8, 16]
	// virus_kills: [n, 8, 16]
	// wishlist: [n, 2, 3, 3, 8, 16]
	// clear_location: [n, 8, 16]
	// clear_pill: [n, 2, 3, 3]
	torch::Tensor priors, valuation, fall_time, occupied, virus_kills, wishlist, clear_location, clear_pill;

	NetOutput() { DebugScope dbg("NetOutput()", DEFAULT_CONSTRUCTOR_VERBOSITY); }

	NetOutput(int n, float *priors_data, float *valuation_data, unsigned char *fall_time_data, char *occupied_data, float *virus_kills_data, float *wishlist_data, float *clear_location_data, float *clear_pill_data) {
		DebugScope dbg("NetOutput(n, ...)", DEFAULT_CONSTRUCTOR_VERBOSITY);
		priors         = torch::from_blob(        priors_data, {n, ROTATIONS, BOARD_WIDTH, BOARD_HEIGHT                   }, [](void *) {}, CPU_FLOAT);
		valuation      = torch::from_blob(     valuation_data, {n                                                         }, [](void *) {}, CPU_FLOAT);
		fall_time      = torch::from_blob(     fall_time_data, {n                                                         }, [](void *) {}, CPU_BYTE );
		occupied       = torch::from_blob(      occupied_data, {n, BOARD_WIDTH, BOARD_HEIGHT                              }, [](void *) {}, CPU_BYTE );
		virus_kills    = torch::from_blob(   virus_kills_data, {n, BOARD_WIDTH, BOARD_HEIGHT                              }, [](void *) {}, CPU_FLOAT);
		wishlist       = torch::from_blob(      wishlist_data, {n, ORIENTATIONS, COLORS, COLORS, BOARD_WIDTH, BOARD_HEIGHT}, [](void *) {}, CPU_FLOAT);
		clear_location = torch::from_blob(clear_location_data, {n, BOARD_WIDTH, BOARD_HEIGHT                              }, [](void *) {}, CPU_FLOAT);
		clear_pill     = torch::from_blob(    clear_pill_data, {n, ORIENTATIONS, COLORS, COLORS                           }, [](void *) {}, CPU_FLOAT);
	}

	NetOutput(int n) {
		DebugScope dbg("NetOutput(n)", DEFAULT_CONSTRUCTOR_VERBOSITY);
		priors         = torch::empty({n, ROTATIONS, BOARD_WIDTH, BOARD_HEIGHT                   }, CPU_FLOAT);
		valuation      = torch::empty({n                                                         }, CPU_FLOAT);
		fall_time      = torch::empty({n                                                         }, CPU_BYTE );
		occupied       = torch::empty({n, BOARD_WIDTH, BOARD_HEIGHT                              }, CPU_BYTE );
		virus_kills    = torch::empty({n, BOARD_WIDTH, BOARD_HEIGHT                              }, CPU_FLOAT);
		wishlist       = torch::empty({n, ORIENTATIONS, COLORS, COLORS, BOARD_WIDTH, BOARD_HEIGHT}, CPU_FLOAT);
		clear_location = torch::empty({n, BOARD_WIDTH, BOARD_HEIGHT                              }, CPU_FLOAT);
		clear_pill     = torch::empty({n, ORIENTATIONS, COLORS, COLORS                           }, CPU_FLOAT);
	}

	void to_gpu() {
		DebugScope dbg("NetOutput::to_gpu", DEFAULT_TRANSFER_VERBOSITY);
		priors         = priors        .to(torch::kCUDA);
		valuation      = valuation     .to(torch::kCUDA);
		fall_time      = fall_time     .to(torch::kCUDA);
		occupied       = occupied      .to(torch::kCUDA);
		virus_kills    = virus_kills   .to(torch::kCUDA);
		wishlist       = wishlist      .to(torch::kCUDA);
		clear_location = clear_location.to(torch::kCUDA);
		clear_pill     = clear_pill    .to(torch::kCUDA);
		fall_time      = fall_time     .to(torch::kF32);
		occupied       = occupied      .to(torch::kF32);
	}

	void to_cpu() {
		DebugScope dbg("NetOutput::to_cpu", DEFAULT_TRANSFER_VERBOSITY);
		priors         = priors        .to(torch::kCPU);
		valuation      = valuation     .to(torch::kCPU);
		fall_time      = fall_time     .to(torch::kCPU);
		occupied       = occupied      .to(torch::kCPU);
		virus_kills    = virus_kills   .to(torch::kCPU);
		wishlist       = wishlist      .to(torch::kCPU);
		clear_location = clear_location.to(torch::kCPU);
		clear_pill     = clear_pill    .to(torch::kCPU);
	}

	void save(torch::serialize::OutputArchive &archive) const {
		DebugScope dbg("NetOutput::save", DEFAULT_SERIALIZATION_VERBOSITY);
		archive.write("priors"        , priors        , true);
		archive.write("valuation"     , valuation     , true);
		archive.write("fall_time"     , fall_time     , true);
		archive.write("occupied"      , occupied      , true);
		archive.write("virus_kills"   , virus_kills   , true);
		archive.write("wishlist"      , wishlist      , true);
		archive.write("clear_location", clear_location, true);
		archive.write("clear_pill"    , clear_pill    , true);
	}

	void load(torch::serialize::InputArchive &archive) {
		DebugScope dbg("NetOutput::load", DEFAULT_SERIALIZATION_VERBOSITY);
		archive.read("priors"        , priors        , true);
		archive.read("valuation"     , valuation     , true);
		archive.read("fall_time"     , fall_time     , true);
		archive.read("occupied"      , occupied      , true);
		archive.read("virus_kills"   , virus_kills   , true);
		archive.read("wishlist"      , wishlist      , true);
		archive.read("clear_location", clear_location, true);
		archive.read("clear_pill"    , clear_pill    , true);
	}

	std::ostream &dump(std::ostream &o) const {
		DebugScope dbg("NetOutput::dump", DEFAULT_SERIALIZATION_VERBOSITY);
		return o
			<<         "priors: " << priors         << std::endl
			<<      "valuation: " << valuation      << std::endl
			<<      "fall_time: " << fall_time      << std::endl
			<<       "occupied: " << occupied       << std::endl
			<<    "virus_kills: " << virus_kills    << std::endl
			<<       "wishlist: " << wishlist       << std::endl
			<< "clear_location: " << clear_location << std::endl
			<<     "clear_pill: " << clear_pill     << std::endl
			;
	}

	std::ostream &sketch(std::ostream &o) const {
		DebugScope dbg("NetOutput::sketch", DEFAULT_SERIALIZATION_VERBOSITY);
		return o
			<<         "priors: " << TensorSketch(priors        ) << ", "
			<<      "valuation: " << TensorSketch(valuation     ) << ", "
			<<      "fall_time: " << TensorSketch(fall_time     ) << ", "
			<<       "occupied: " << TensorSketch(occupied      ) << ", "
			<<    "virus_kills: " << TensorSketch(virus_kills   ) << ", "
			<<       "wishlist: " << TensorSketch(wishlist      ) << ", "
			<< "clear_location: " << TensorSketch(clear_location) << ", "
			<<     "clear_pill: " << TensorSketch(clear_pill    )
			;
	}
};

std::ostream &operator<<(std::ostream &o, const NetOutput &out) { return out.dump(o); }

struct Batch {
	NetInput  in;
	NetOutput out;
	// reachable: [n, 4, 8, 16]
	torch::Tensor reachable;

	Batch() { DebugScope dbg("Batch()", DEFAULT_CONSTRUCTOR_VERBOSITY); }

	Batch(int n): in(n), out(n) {
		DebugScope dbg("Batch(n)", DEFAULT_CONSTRUCTOR_VERBOSITY);
		reachable = torch::empty({n, ROTATIONS, BOARD_WIDTH, BOARD_HEIGHT}, CPU_BYTE);
	}

	Batch(int n,
		char *reachable_data,
		float *priors_data, float *valuation_data, unsigned char *fall_time_data, char *occupied_data, float *virus_kills_data, float *wishlist_data, float *clear_location_data, float *clear_pill_data,
		char *boards_data, char *lookaheads_data, float *scalars_data
		)
		: in(n, boards_data, lookaheads_data, scalars_data)
		, out(n, priors_data, valuation_data, fall_time_data, occupied_data, virus_kills_data, wishlist_data, clear_location_data, clear_pill_data) {
		DebugScope dbg("Batch(n, ...)", DEFAULT_CONSTRUCTOR_VERBOSITY);
		reachable = torch::from_blob(reachable_data, {n, ROTATIONS, BOARD_WIDTH, BOARD_HEIGHT}, [](void *){}, CPU_BYTE);
	}

	void to_gpu() {
		DebugScope dbg("Batch::to_gpu", DEFAULT_TRANSFER_VERBOSITY);
		in.to_gpu();
		out.to_gpu();
		reachable = reachable.to(torch::kCUDA);
		reachable = reachable.to(torch::kF32);
	}

	void save(torch::serialize::OutputArchive &archive) {
		DebugScope dbg("Batch::save", DEFAULT_SERIALIZATION_VERBOSITY);
		in.save(archive);
		out.save(archive);
		archive.write("reachable", reachable, true);
	}

	void load(torch::serialize::InputArchive &archive) {
		DebugScope dbg("Batch::load", DEFAULT_SERIALIZATION_VERBOSITY);
		in.load(archive);
		out.load(archive);
		archive.read("reachable", reachable, true);
	}

	std::ostream &dump(std::ostream &o) const {
		DebugScope dbg("Batch::dump", DEFAULT_SERIALIZATION_VERBOSITY);
		return o
			<< in
			<< out
			<< "reachable: " << reachable << std::endl;
	}

	std::ostream &sketch(std::ostream &o) const {
		DebugScope dbg("Batch::sketch", DEFAULT_SERIALIZATION_VERBOSITY);
		in.sketch(o) << ", ";
		out.sketch(o) << ", ";
		return o << "reachable: " << TensorSketch(reachable);
	}
};

std::ostream &operator<<(std::ostream &o, const Batch &batch) { return batch.dump(o); }

class Conv2dReLUInitImpl : public torch::nn::Module {
	public:
		Conv2dReLUInitImpl(int64_t iChans, int64_t oChans, float leakage, int64_t padding=1, int64_t k=3, bool bias=true)
			: torch::nn::Module("Conv2d (custom initialization)")
			, conv(torch::nn::Conv2d(torch::nn::Conv2dOptions(iChans, oChans, k).padding(padding).bias(bias)))
		{
			DebugScope dbg("Conv2dReLUInitImpl(...)", DEFAULT_CONSTRUCTOR_VERBOSITY);
			// scaling factor is from Delving Deep into Rectifiers
			auto scaling = std::sqrt(2 / ((1 + leakage*leakage) * k*k*iChans));
			torch::autograd::GradMode::set_enabled(false);
			conv->weight.normal_(0, scaling);
			if(bias) conv->bias.normal_(0, 1);
			torch::autograd::GradMode::set_enabled(true);
			register_module("conv", conv);
		}

		torch::Tensor forward(const torch::Tensor &in) {
			DebugScope dbg("Conv2dReLUInitImpl::forward", DEFAULT_LAYER_VERBOSITY);
			return conv->forward(in);
		}

		// TODO: better dimensions for board-sized convolutions
		sp_endpoint weights() {
			DebugScope dbg("Conv2dReLUInitImpl::weights");
			return generic_module_weights(conv);
		}

		torch::nn::Conv2d conv;
};
TORCH_MODULE(Conv2dReLUInit);

// Residual design is inspired by Fixup Initialization: Residual Learning Without Normalization
class ResidualImpl : public torch::nn::Module {
	public:
		ResidualImpl(int64_t chans, float leakage, int64_t padding=1, int64_t k=3)
			: torch::nn::Module("residual block")
			, conv0(chans, chans, leakage, padding, k)
			, conv1(chans, chans, leakage, padding, k, false)
			, norm0(torch::nn::BatchNorm2dOptions(chans))
			, norm1(torch::nn::BatchNorm2dOptions(chans))
			, lrelu(torch::nn::LeakyReLU(torch::nn::LeakyReLUOptions().negative_slope(leakage)))
		{
			DebugScope dbg("ResidualImpl(...)", DEFAULT_CONSTRUCTOR_VERBOSITY);

			torch::autograd::GradMode::set_enabled(false);
			conv0->conv->weight /= std::sqrt(RESIDUAL_BLOCKS);
			conv1->conv->weight.zero_();
			torch::autograd::GradMode::set_enabled(true);

			register_module("conv0", conv0);
			register_module("conv1", conv1);
			register_module("norm0", norm0);
			register_module("norm1", norm1);
			register_module("lrelu", lrelu);
		}

		torch::Tensor forward(const torch::Tensor &in) {
			DebugScope dbg("ResidualImpl::forward", DEFAULT_LAYER_VERBOSITY);
			return lrelu->forward(0.9*in + 0.1*
				norm1->forward(
				conv1->forward(
				lrelu->forward(
				norm0->forward(
				conv0->forward(
				in))))));
		}

		sp_endpoint weights() {
			DebugScope dbg("ResidualImpl::weights");
			sp_endpoint out(new _endpoint);
			// conv0 and conv1 are different enough that this can't be a two-element vector of dictionaries
			out->tag = tag_dictionary;
			out->size = 1;
			out->add_child("norm0", generic_module_weights(norm0));
			out->add_child("norm1", generic_module_weights(norm1));
			out->add_child("conv0", conv0->weights());
			out->add_child("conv1", conv1->weights());
			return out;
		}

	Conv2dReLUInit conv0, conv1;
	torch::nn::BatchNorm2d norm0, norm1;
	torch::nn::LeakyReLU lrelu;
};
TORCH_MODULE(Residual);

class Encoder;
class EncoderImpl : public torch::nn::Module {
	public:
		EncoderImpl(sp_structure s, std::string name = "Encoder");
		torch::Tensor forward(sp_endpoint e);
		sp_endpoint weights();

	private:
		sp_structure shape;
		torch::nn::Linear linear;
		Conv2dReLUInit convolution;
		std::map<std::string, Encoder> dict;
		std::vector<Encoder> vec;
};
TORCH_MODULE(Encoder);

class Decoder;
class DecoderImpl : public torch::nn::Module {
	public:
		DecoderImpl(sp_structure s, std::string name = "Decoder");
		sp_endpoint forward(torch::Tensor t);
		sp_endpoint weights();
		sp_structure shape;

	private:
		torch::nn::Linear linear;
		Conv2dReLUInit convolution;
		std::map<std::string, Decoder> dict;
		std::vector<Decoder> vec;
};
TORCH_MODULE(Decoder);

bool is_board(std::vector<game_constant> dims) {
	auto n = dims.size();
	return n >= 2 && dims[n-2] == c_width && dims[n-1] == c_height;
}

EncoderImpl::EncoderImpl(sp_structure s, std::string name)
	: linear(nullptr), convolution(nullptr), shape(s)
{
	DebugScope dbg("EncoderImpl()", DEFAULT_CONSTRUCTOR_VERBOSITY);
	switch(s->tag) {
		case tag_tensor:
			if(is_board(s->dims)) {
				convolution = Conv2dReLUInit(eval_game_constants(s->dims)/CELLS, FILTERS, LEAKAGE);
				register_module(name + ":convolution", convolution);
			} else {
				linear = torch::nn::Linear(eval_game_constants(s->dims), FILTERS);
				register_module(name + ":linear", linear);
			}
			break;
		case tag_vector:
			for(int i = 0; i < eval_game_constant(s->dims[0]); i++) {
				auto child_name = name + "." + std::to_string(i);
				auto child = Encoder(s->vec, child_name);
				vec.push_back(child);
				register_module(child_name, child);
			}
			break;
		case tag_dictionary:
			for(auto pair: s->dict) {
				// TODO: escape pair.first, I guess
				auto child_name = name + ":" + pair.first;
				auto child = Encoder(pair.second, child_name);
				dict.emplace(pair.first, child);
				register_module(child_name, child);
			}
			break;
		default:
			std::cerr << "Invalid tag " << s->tag << " in EncoderImpl()." << std::endl;
			throw 0;
	}
}

DecoderImpl::DecoderImpl(sp_structure s, std::string name)
	: linear(nullptr), convolution(nullptr), shape(s)
{
	DebugScope dbg("DecoderImpl()", DEFAULT_CONSTRUCTOR_VERBOSITY);
	switch(s->tag) {
		case tag_tensor:
			if(is_board(s->dims)) {
				convolution = Conv2dReLUInit(FILTERS, eval_game_constants(s->dims)/CELLS, LEAKAGE);
				register_module(name + ":convolution", convolution);
			} else {
				linear = torch::nn::Linear(BODY_SIZE, eval_game_constants(s->dims));
				register_module(name + ":linear", linear);
			}
			break;
		case tag_vector:
			for(int i = 0; i < eval_game_constant(s->dims[0]); i++) {
				auto child_name = name + "." + std::to_string(i);
				auto child = Decoder(s->vec, child_name);
				vec.push_back(child);
				register_module(child_name, child);
			}
			break;
		case tag_dictionary:
			for(auto pair: s->dict) {
				// TODO: escape pair.first, I guess
				auto child_name = name + ":" + pair.first;
				auto child = Decoder(pair.second, child_name);
				dict.emplace(pair.first, child);
				register_module(child_name, child);
			}
			break;
		default:
			std::cerr << "Invalid tag " << s->tag << " in DecoderImpl()." << std::endl;
			throw 0;
	}
}

torch::Tensor EncoderImpl::forward(sp_endpoint e) {
	DebugScope dbg("EncoderImpl::forward");
	const int n = e->size;
	switch(e->tag) {
		case tag_tensor: {
				int64_t sz_per_batch = eval_game_constants(e->dims);
				if(is_board(e->dims)) {
					return convolution->forward(e->values.reshape({n, sz_per_batch/CELLS, BOARD_WIDTH, BOARD_HEIGHT}));
				} else {
					return linear
						->forward(e->values.reshape({n, sz_per_batch}))
						.reshape({n, FILTERS, 1, 1})
						.expand({n, FILTERS, BOARD_WIDTH, BOARD_HEIGHT});
				}
			}
		case tag_vector: {
			torch::Tensor sum = torch::zeros({n, FILTERS, BOARD_WIDTH, BOARD_HEIGHT}, GPU_FLOAT);
			for(int i = 0; i < vec.size(); ++i)
				sum += vec[i]->forward(e->vec[i]);
			return sum;
			}
		case tag_dictionary: {
			torch::Tensor sum = torch::zeros({n, FILTERS, BOARD_WIDTH, BOARD_HEIGHT}, GPU_FLOAT);
			for(auto [nm, child]: dict)
				sum += child->forward(e->dict[nm]);
			return sum;
			}
		default:
			std::cerr << "Invalid tag " << e->tag << " in EncoderImpl::forward()." << std::endl;
			throw 0;
	}
}

sp_endpoint DecoderImpl::forward(torch::Tensor t) {
	DebugScope dbg("DecoderImpl::forward");

	auto e = new _endpoint();
	e->size = t.size(0);
	e->tag = shape->tag;
	e->dims = shape->dims;

	switch(shape->tag) {
		case tag_tensor:
			e->values = is_board(shape->dims)
				? convolution->forward(t)
				: linear->forward(t.reshape({e->size, BODY_SIZE}));
			e->values = e->values.reshape(tensor_dimensions(e->size, shape->dims));
			switch(shape->ty) {
				case type_probability: [[fallthrough]];
				case type_unit: e->values = e->values.sigmoid(); break;
				case type_positive: [[fallthrough]];
				case type_categorical: e->values = e->values.exp(); break;
				default:
					std::cerr << "Invalid leaf type " << shape->ty << " in DecoderImpl::forward()." << std::endl;
					throw 0;
				}
			break;
		case tag_vector:
			for(int i = 0; i < vec.size(); i++)
				e->vec.push_back(vec[i]->forward(t));
			break;
		case tag_dictionary:
			for(auto [nm, child]: dict)
				e->dict[nm] = child->forward(t);
			break;
		default:
			std::cerr << "Invalid tag " << shape->tag << " in DecoderImpl::forward()." << std::endl;
			throw 0;
	}

	return sp_endpoint(e);
}

sp_endpoint tensor_endpoint(torch::Tensor t) {
	DebugScope dbg("tensor_endpoint");
	sp_endpoint e(new _endpoint);
	e->tag = tag_tensor;
	e->size = t.size(0);
	for(int i = 1; i < t.dim(); ++i) e->dims.push_back(~t.size(i));
	e->values = t;
	return e;
}

sp_endpoint multi_tensor_endpoint(const std::vector<torch::Tensor> &v) {
	DebugScope dbg("multi_tensor_endpoint(v)");
	switch(v.size()) {
		case 0: return sp_endpoint();
		case 1: return tensor_endpoint(v[0].unsqueeze(0));
		default:
			bool all_same_size = true;
			for(int i = 1; all_same_size && i < v.size(); ++i)
				all_same_size = all_same_size && v[0].is_same_size(v[i]);

			if(all_same_size) {
				std::vector<torch::Tensor> unsquoze;
				for(auto t: v) unsquoze.push_back(t.unsqueeze(0));
				return tensor_endpoint(at::cat(unsquoze, 0).unsqueeze(0));
			} else {
				sp_endpoint e(new _endpoint);
				e->tag = tag_dictionary;
				e->size = 1;
				for(int i = 0; i < v.size(); i++)
					e->add_child(std::to_string(i), tensor_endpoint(v[i].unsqueeze(0)));
				return e;
			}
	}
}

template<typename T>
sp_endpoint generic_module_weights(T m) {
	DebugScope dbg("generic_module_weights");

	sp_endpoint params = multi_tensor_endpoint(m->parameters(true)), buffers = multi_tensor_endpoint(m->buffers(true));
	if(params == nullptr && buffers == nullptr) {
		std::cerr << "Attempted to extract weights from a module with no buffers or parameters." << std::endl;
		throw 0;
	}
	if(params == nullptr) return buffers;
	if(buffers == nullptr) return params;

	sp_endpoint out(new _endpoint);
	out->size = 1;
	out->tag = tag_dictionary;
	out->add_child("parameters", params);
	out->add_child("buffers", buffers);
	return out;
}

sp_endpoint EncoderImpl::weights() {
	DebugScope dbg("EncoderImpl::weights");
	sp_endpoint e(new _endpoint);
	e->tag = shape->tag;
	e->size = 1;

	switch(shape->tag) {
		case tag_tensor:
			// TODO: return better game constants describing these shapes
			if(is_board(shape->dims)) return convolution->weights();
			else return generic_module_weights(linear);
		case tag_vector:
			e->dims = shape->dims;
			for(auto child: vec) e->vec.push_back(child->weights());
			return e;
		case tag_dictionary:
			for(auto pair: dict)
				e->add_child(pair.first, pair.second->weights());
			return e;
		default:
			std::cerr << "Invalid tag " << shape->tag << " in EncoderImpl::weights()." << std::endl;
			throw 0;
	}
}

sp_endpoint DecoderImpl::weights() {
	DebugScope dbg("DecoderImpl::weights");
	sp_endpoint e(new _endpoint);
	e->tag = shape->tag;
	e->size = 1;

	switch(shape->tag) {
		case tag_tensor:
			if(is_board(shape->dims)) return convolution->weights();
			// TODO: better dimensions
			else return generic_module_weights(linear);
		case tag_vector:
			for(auto child: vec) e->vec.push_back(child->weights());
			return e;
		case tag_dictionary:
			for(auto pair: dict) e->add_child(pair.first, pair.second->weights());
			return e;
		default:
			std::cerr << "Invalid tag " << shape->tag << " in DecoderImpl::weights()." << std::endl;
			throw 0;
	}
}

class NextNetImpl : public torch::nn::Module {
	public:
		NextNetImpl(sp_structure in, sp_structure out)
			: torch::nn::Module("Nurse Sveta net")
			, enc(in)
			, dec(out)
		{
			DebugScope dbg("NextNetImpl()", DEFAULT_CONSTRUCTOR_VERBOSITY);

			register_module("encoder", enc);
			for(int64_t i = 0; i < RESIDUAL_BLOCKS; i++) {
				residuals.push_back(Residual(FILTERS, LEAKAGE));
				register_module("main body residual #" + std::to_string(i), residuals[i]);
			}
			register_module("decoder", dec);

			to(torch::kCUDA);
			to(torch::kF32);
		}

		sp_endpoint forward(sp_endpoint in) {
			DebugScope dbg("NextNetImpl::forward");
			torch::Tensor t = enc->forward(in);
			for(int i = 0; i < RESIDUAL_BLOCKS; i++) t = residuals[i]->forward(t);
			return dec->forward(t);
		}

		sp_endpoint weights() {
			DebugScope dbg("NextNetImpl::weights");

			auto res = sp_endpoint(new _endpoint);
			res->tag = tag_vector;
			res->size = 1;
			res->dims.push_back(~residuals.size());
			res->vec.reserve(residuals.size());
			for(auto residual: residuals) {
				res->vec.push_back(residual->weights());
			}

			auto top = sp_endpoint(new _endpoint);
			top->tag = tag_dictionary;
			top->size = 1;
			top->add_child("encoder", enc->weights());
			top->add_child("residuals", res);
			top->add_child("decoder", dec->weights());

			return top;
		}

		Encoder enc;
		std::vector<Residual> residuals;
		Decoder dec;
};
TORCH_MODULE(NextNet);

torch::Tensor next_leaf_loss(leaf_type loss_ty, const torch::Tensor &ground_truth, const torch::Tensor *const mask, const torch::Tensor &net_output) {
	DebugScope dbg("next_leaf_loss");

	if(ground_truth.sizes() != net_output.sizes() || (mask != nullptr && ground_truth.sizes() != mask->sizes())) {
		std::cerr << "Mismatched tensor shapes in next_leaf_loss" << std::endl;
		std::cerr << "Ground truth: " << TensorSketch(ground_truth) << std::endl;
		std::cerr << "Net output: " << TensorSketch(net_output) << std::endl;
		if(mask != nullptr) std::cerr << "Mask: " << TensorSketch(*mask) << std::endl;
		throw 0;
	}

	if(CHECK_MASK_VALIDITY && mask != nullptr && !ground_truth.equal(ground_truth * *mask)) {
		std::cerr << "Invalid mask found in next_leaf_loss: some masked values were nonzero." << std::endl;
		std::cerr << "Values: " << ground_truth << std::endl;
		std::cerr << "Mask: " << *mask << std::endl;
		throw 0;
	}

	switch(loss_ty) {
		case type_unit: [[fallthrough]];
		case type_positive:
			return mask == nullptr ?
				(ground_truth - net_output).square() :
				(ground_truth - net_output * *mask).square();
		case type_categorical: {
			std::vector<int64_t> sum_dimensions; sum_dimensions.reserve(ground_truth.dim() - 1);
			for(int i = 1; i < ground_truth.dim(); ++i) sum_dimensions.push_back(i);
			torch::Tensor normalization =
				mask == nullptr ?
				net_output.sum(sum_dimensions, true) :
				(net_output * *mask).sum(sum_dimensions, true);

			// ground_truth can have zeros (e.g. everywhere the mask is zero), which
			// leads to -inf's after the log, which leads to nan's in the gradients. We
			// bring zeros up to EPSILON to avoid this. I also tried clamping after the
			// log, but the nan's show up even after multiplying by the mask to screen
			// off the effect of the incoming zeros.
			//
			// ground_truth is zero everywhere mask is, so we don't need to mask again.
			// TODO: should this be t*log(t/o) or o*log(o/t)?
			return ground_truth * (ground_truth.clamp_min(EPSILON) * normalization / net_output).log();
			}
		case type_probability: {
			torch::Tensor clamped_output = net_output.clamp(EPSILON, 1-EPSILON);
			if(CHECK_NO_CLAMPING && !clamped_output.equal(net_output))
				std::cerr << "WARNING: net output a probability so confident that it was clamped" << std::endl;
			torch::Tensor first_cross_entropy_term = ((ground_truth - 1) * (1 - clamped_output).log());
			if(mask != nullptr) first_cross_entropy_term *= *mask;
			return first_cross_entropy_term - ground_truth * clamped_output.log();
			}
		default:
			std::cerr << "Invalid loss type " << loss_ty << " in next_leaf_loss." << std::endl;
			throw 0;
	}
}

class NetImpl : public torch::nn::Module {
	public:
		NetImpl()
			: torch::nn::Module("Nurse Sveta net")
			, board_convolution(CELL_SIZE, FILTERS, LEAKAGE)
			, lookahead_linear(LOOKAHEAD_SIZE, FILTERS)
			, scalar_linear(SCALARS, FILTERS)
			, input_norm(torch::nn::BatchNorm2dOptions(FILTERS))
			, output_convolution(FILTERS, OUTPUT_LAYERS, LEAKAGE, 0, 1)
			, output_linear(CELLS, OUTPUT_SCALARS)
			{
				DebugScope dbg("NetImpl()", DEFAULT_CONSTRUCTOR_VERBOSITY);
				register_module("initial board convolution", board_convolution);
				register_module("fully-connected feed for lookaheads", lookahead_linear);
				register_module("fully-connected feed for game history statistics", scalar_linear);
				register_module("input normalization", input_norm);

				for(int64_t i = 0; i < RESIDUAL_BLOCKS; i++) {
					std::stringstream ss;
					ss << "main body residual #" << i;
					residuals.push_back(Residual(FILTERS, LEAKAGE));
					register_module(ss.str(), residuals[i]);
				}

				register_module("output convolution", output_convolution);
				register_module("output linear", output_linear);

				to(torch::kCUDA);
				to(torch::kF32);
			}

		NetOutput forward(const NetInput &in);

	private:
		Conv2dReLUInit board_convolution, output_convolution;
		torch::nn::Linear lookahead_linear, scalar_linear;
		torch::nn::BatchNorm2d input_norm;
		std::vector<Residual> residuals;
		torch::nn::Linear output_linear;
};
TORCH_MODULE(Net);

NetOutput NetImpl::forward(const NetInput &in) {
	DebugScope dbg("NetImpl::forward");
	const int64_t n = in.boards.size(0);
	torch::Tensor t = input_norm->forward(
		board_convolution->forward(in.boards) +
		lookahead_linear->forward(in.lookaheads).reshape({n, FILTERS, 1, 1}).expand({n, FILTERS, BOARD_WIDTH, BOARD_HEIGHT}) +
		   scalar_linear->forward(in.scalars   ).reshape({n, FILTERS, 1, 1}).expand({n, FILTERS, BOARD_WIDTH, BOARD_HEIGHT})
		);
	for(int64_t i = 0; i < RESIDUAL_BLOCKS; i++) t = residuals[i]->forward(t);
	t = output_convolution->forward(t);

	NetOutput out;
	int64_t i = 0;
	auto all = torch::indexing::Slice();

	out.priors         = t.index({all, torch::indexing::Slice(i, i+ROTATIONS   ), "..."}).exp(); i+=ROTATIONS;
	out.wishlist       = t.index({all, torch::indexing::Slice(i, i+PILLCONTENTS), "..."}).reshape({n, ORIENTATIONS, COLORS, COLORS, BOARD_WIDTH, BOARD_HEIGHT}).sigmoid(); i+=PILLCONTENTS;
	out.occupied       = t.index({all, i, "..."}).reshape({n, BOARD_WIDTH, BOARD_HEIGHT}).sigmoid(); i++;
	out.virus_kills    = t.index({all, i, "..."}).reshape({n, BOARD_WIDTH, BOARD_HEIGHT}).sigmoid(); i++;
	out.clear_location = t.index({all, i, "..."}).reshape({n, BOARD_WIDTH, BOARD_HEIGHT}).sigmoid(); i++;

	t = output_linear->forward(t.index({all, i, "..."}).reshape({n, CELLS})); i++;
	if(i != OUTPUT_LAYERS) throw 0;

	i = 0;
	out.clear_pill = t.index({all, torch::indexing::Slice(i, i+PILLCONTENTS)}).reshape({n, ORIENTATIONS, COLORS, COLORS}).sigmoid(); i+=PILLCONTENTS;
	out.valuation  = t.index({all, i}).reshape({n}).sigmoid(); i++;
	out.fall_time  = t.index({all, i}).reshape({n}); i++;
	if(i != OUTPUT_SCALARS) throw 0;

	return out;
}

torch::Tensor detailed_loss(Net &net, const Batch &batch, float *loss_scaling_array) {
	DebugScope dbg("detailed_loss(net, batch, loss_scaling)");
	const int n = batch.out.priors.size(0);
	auto net_out = net->forward(batch.in);
	auto loss = torch::zeros({OUTPUT_TYPES}, GPU_FLOAT);
	auto loss_scaling_tensor = torch::from_blob(loss_scaling_array, {OUTPUT_TYPES}, [](void *v){}, CPU_FLOAT).to(torch::kCUDA);
	int64_t i = 0;
	// Kullback-Leibler divergence for priors
	auto scaled_priors = net_out.priors / (batch.reachable*net_out.priors).sum({1,2,3}, true);
	// correct_out.priors can have zeros (e.g. everywhere reachable is zero),
	// which leads to -inf's after the log, which leads to nan's in the
	// gradients. I also tried clamping after the log, but the nan's show up
	// even after multiplying by reachable to screen off the effect of the
	// incoming zeros.
	loss.index_put_({i++}, (batch.out.priors * (batch.out.priors.clamp_min(EPSILON) / scaled_priors).log() * batch.reachable).sum());
	// cross-entropy loss for valuation
	auto bs = net_out.valuation.clamp(EPSILON, 1-EPSILON);
	loss.index_put_({i++}, (batch.out.valuation * bs.log() + (1 - batch.out.valuation) * (1 - bs).log()).sum().neg());
	// squared-error loss for everything else
	loss.index_put_({i++}, (batch.out.fall_time      - net_out.fall_time     ).square().sum());
	loss.index_put_({i++}, (batch.out.occupied       - net_out.occupied      ).square().sum());
	loss.index_put_({i++}, (batch.out.virus_kills    - net_out.virus_kills   ).square().sum());
	loss.index_put_({i++}, (batch.out.wishlist       - net_out.wishlist      ).square().sum());
	loss.index_put_({i++}, (batch.out.clear_location - net_out.clear_location).square().sum());
	loss.index_put_({i++}, (batch.out.clear_pill     - net_out.clear_pill    ).square().sum());
	if(i != OUTPUT_TYPES) throw 0;
	loss /= n;
	loss *= loss_scaling_tensor;
	return loss;
}

extern "C" {
	NextNet *next_sample_net(bool training, structure *in, structure *out);
	void next_discard_net(NextNet *net);
	endpoint *next_evaluate_net(NextNet *net, endpoint *in);
	endpoint *next_net_weights(NextNet *net);
}

NextNet *next_sample_net(bool training, structure *in, structure *out) {
	NextNet *net = new NextNet(in->ref, out->ref);
	// TODO: does this do everything we want? turn off autograd e.g.?
	(*net)->train(training);
	return net;
}

void next_discard_net(NextNet *net) { delete net; }

endpoint *next_evaluate_net(NextNet *net, endpoint *in) {
	// TODO: think very very hard about where exactly we want gradient control and training control in the API
	torch::NoGradGuard g;
	return new endpoint((*net)->forward(in->ref));
}

endpoint *next_net_weights(NextNet *net) {
	return new endpoint((*net)->weights());
}

void tensorcpy(float *out, const torch::Tensor &in) {
	DebugScope dbg("tensorcpy", DEFAULT_SERIALIZATION_VERBOSITY);
	if(out == NULL) return;
	if(in.dtype() != torch::kF32) throw 0;
	int64_t len = 1;
	for(int i = 0; i < in.dim(); i++) len *= in.size(i);

	// TODO: is it safe to inline the definition of contig_in, or will that
	// lead to the Tensor being destructed before memcpy finishes?
	auto contig_in = in.to(torch::kCPU).contiguous();
	std::memcpy(out, contig_in.data_ptr<float>(), len*sizeof(float));
}

extern "C" {
	Net *sample_net(bool training);
	void save_net(Net *net, torch::optim::SGD *optim, char *path);
	void load_net(char *path, Net **net, torch::optim::SGD **optim);
	void discard_net(Net *net);
	void evaluate_net(Net *net, int n, float *priors, float *valuation, char *boards, char *lookaheads, float *scalars);
	void save_example(char *path, char *reachable, float *priors, float valuation, unsigned char fall_time, char *occupied, float *virus_kills, float *wishlist, float *clear_location, float *clear_pill, char *board, char *lookahead, float *scalars);
	Batch *load_batch(char **path, int n);
	int batch_size(Batch *batch);
	void discard_batch(Batch *batch);
	float train_net(Net *net, torch::optim::SGD *optim, Batch *batch, float *loss_scaling);
	void introspect_net(Net *net, Batch *batch, float *priors, float *valuation, float *fall_time, float *occupied, float *virus_kills, float *wishlist, float *clear_location, float *clear_pill);
	void detailed_loss(Net *net, float *out, Batch *batch, float *loss_scaling);
	torch::optim::SGD *connect_optimizer(Net *net);
	void discard_optimizer(torch::optim::SGD *optim);
}

Net *sample_net(bool training) {
	DebugScope dbg("sample_net", DEFAULT_CONSTRUCTOR_VERBOSITY);
	Net *net = new Net();
	// TODO: does this do everything we want? turn off autograd e.g.?
	(**net).train(training);
	return net;
}
void discard_net(Net *net) {
	DebugScope dbg("discard_net", DEFAULT_CONSTRUCTOR_VERBOSITY);
	delete net;
}

void save_net(Net *net, torch::optim::SGD *optim, char *path) {
	DebugScope dbg("save_net", DEFAULT_SERIALIZATION_VERBOSITY);
	torch::serialize::OutputArchive archive;
	(**net).save(archive);
	optim->save(archive);
	archive.save_to(path);
}

void load_net(char *path, Net **netptr, torch::optim::SGD **optimptr) {
	DebugScope dbg("load_net", DEFAULT_SERIALIZATION_VERBOSITY);
	torch::serialize::InputArchive archive;
	archive.load_from(path);

	*netptr = new Net();
	auto net = **netptr;
	net->load(archive);
	if(optimptr == NULL) {
		net->train(false);
	} else {
		net->train(true);
		// TODO: load SGD parameters, they aren't saved with the SGD state FFS
		*optimptr = new torch::optim::SGD(net->parameters(), INITIAL_LEARNING_RATE);
		(**optimptr).load(archive);
	}
}

void evaluate_net(Net *net, int n, float *priors, float *valuation, char *boards, char *lookaheads, float *scalars) {
	DebugScope dbg("evaluate_net");
	torch::NoGradGuard g;
	NetInput in(n, boards, lookaheads, scalars); in.to_gpu();
	NetOutput out = (**net).forward(in);

	tensorcpy(priors,    out.priors);
	tensorcpy(valuation, out.valuation);
}

void save_example(char *path, char *reachable,
	float *priors, float valuation, unsigned char fall_time, char *occupied, float *virus_kills, float *wishlist, float *clear_location, float *clear_pill,
	char *board, char *lookahead, float *scalars) {
	DebugScope dbg("save_example", DEFAULT_SERIALIZATION_VERBOSITY);
	Batch batch(1, reachable, priors, &valuation, &fall_time, occupied, virus_kills, wishlist, clear_location, clear_pill, board, lookahead, scalars);
	torch::serialize::OutputArchive archive;
	batch.save(archive);
	archive.save_to(path);
}

Batch *load_batch(char **path, int n) {
	DebugScope dbg("load_batch", DEFAULT_SERIALIZATION_VERBOSITY);
	Batch *batch = new Batch(n), example;
	torch::serialize::InputArchive archive;

	for(int i = 0; i < n; i++) {
		archive.load_from(path[i]);
		example.load(archive);
		batch->in .boards        .index_put_({i, "..."}, example.in .boards        );
		batch->in .lookaheads    .index_put_({i, "..."}, example.in .lookaheads    );
		batch->in .scalars       .index_put_({i, "..."}, example.in .scalars       );
		batch->out.priors        .index_put_({i, "..."}, example.out.priors        );
		batch->out.valuation     .index_put_({i, "..."}, example.out.valuation     );
		batch->out.fall_time     .index_put_({i, "..."}, example.out.fall_time     );
		batch->out.occupied      .index_put_({i, "..."}, example.out.occupied      );
		batch->out.virus_kills   .index_put_({i, "..."}, example.out.virus_kills   );
		batch->out.wishlist      .index_put_({i, "..."}, example.out.wishlist      );
		batch->out.clear_location.index_put_({i, "..."}, example.out.clear_location);
		batch->out.clear_pill    .index_put_({i, "..."}, example.out.clear_pill    );
		batch->    reachable     .index_put_({i, "..."}, example.    reachable     );
	}

	batch->to_gpu();
	return batch;
}

int batch_size(Batch *batch) {
	DebugScope dbg("batch_size");
	return batch->reachable.size(0);
}

void discard_batch(Batch *batch) {
	DebugScope dbg("discard_batch", DEFAULT_CONSTRUCTOR_VERBOSITY);
	delete batch;
}

float train_net(Net *net, torch::optim::SGD *optim, Batch *batch, float *loss_scaling) {
	DebugScope dbg("train_net");
	optim->zero_grad();
	auto loss = detailed_loss(*net, *batch, loss_scaling).sum();
	loss.backward();
	optim->step();
	return loss.item<float>();
}

void introspect_net(Net *net, Batch *batch, float *priors, float *valuation, float *fall_time, float *occupied, float *virus_kills, float *wishlist, float *clear_location, float *clear_pill) {
	DebugScope dbg("introspect_net");
	torch::NoGradGuard g;
	bool was_training = (**net).is_training();
	(**net).train(false);

	auto out = (*net)->forward(batch->in);

	tensorcpy(priors        , out.priors        );
	tensorcpy(valuation     , out.valuation     );
	tensorcpy(fall_time     , out.fall_time     );
	tensorcpy(occupied      , out.occupied      );
	tensorcpy(virus_kills   , out.virus_kills   );
	tensorcpy(wishlist      , out.wishlist      );
	tensorcpy(clear_location, out.clear_location);
	tensorcpy(clear_pill    , out.clear_pill    );

	(**net).train(was_training);
}

void detailed_loss(Net *net, float *out, Batch *batch, float *loss_scaling) {
	DebugScope dbg("detailed_loss(net, out, batch, loss_scaling)");
	torch::NoGradGuard g;
	bool was_training = (**net).is_training();
	(**net).train(false); // don't want to accidentally teach our BatchNorm layers about our test vectors...
	tensorcpy(out, detailed_loss(*net, *batch, loss_scaling));
	(**net).train(was_training);
}

torch::optim::SGD *connect_optimizer(Net *net) {
	DebugScope dbg("connect_optimizer", DEFAULT_CONSTRUCTOR_VERBOSITY);
	// TODO: allow setting SGD parameters like momentum, learning rate, etc.
	return new torch::optim::SGD((**net).parameters(), INITIAL_LEARNING_RATE);
}

void discard_optimizer(torch::optim::SGD *optim) {
	DebugScope dbg("discard_optimizer", DEFAULT_CONSTRUCTOR_VERBOSITY);
	delete optim;
}
