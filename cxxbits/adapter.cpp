#include "constants.hpp"
#include "debugging.hpp"
#include "endpoint.hpp"

const int64_t CELLS = BOARD_WIDTH * BOARD_HEIGHT;

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

sp_endpoint tensor_endpoint(const torch::Tensor &t);
template<typename T> sp_endpoint generic_module_weights(T m);

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
			return lrelu->forward(in +
				norm1->forward(
				conv1->forward(
				lrelu->forward(
				norm0->forward(
				conv0->forward(
				in))))));
		}

		sp_endpoint activations(const torch::Tensor &in) {
			DebugScope dbg("ResidualImpl::activations", DEFAULT_LAYER_VERBOSITY);

			auto conv0_out = conv0->forward(in),
			     norm0_out = norm0->forward(conv0_out),
			     lrelu_out = lrelu->forward(norm0_out),
			     conv1_out = conv1->forward(lrelu_out),
			     norm1_out = norm1->forward(conv1_out),
			     skip_out = in + norm1_out,
			     final_out = lrelu->forward(skip_out);

			sp_endpoint out(new _endpoint);
			out->tag = tag_dictionary;
			out->size = in.size(0);
			// TODO: use c_width and c_height in appropriate places
			out->add_child("conv0", tensor_endpoint(conv0_out));
			out->add_child("norm0", tensor_endpoint(norm0_out));
			out->add_child("lrelu", tensor_endpoint(lrelu_out));
			out->add_child("conv1", tensor_endpoint(conv1_out));
			out->add_child("norm1", tensor_endpoint(norm1_out));
			out->add_child("skip", tensor_endpoint(skip_out));
			out->add_child("output", tensor_endpoint(final_out));

			return out;
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
		sp_endpoint activations(sp_endpoint e);
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
		sp_endpoint forward(const torch::Tensor &t);
		sp_endpoint activations(const torch::Tensor &t) { return forward(t); }
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
	DebugScope dbg("EncoderImpl::forward", DEFAULT_LAYER_VERBOSITY);
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

sp_endpoint EncoderImpl::activations(sp_endpoint e) {
	DebugScope dbg("EncoderImpl::activations", DEFAULT_LAYER_VERBOSITY);
	const int n = e->size;
	torch::Tensor out_t;
	sp_endpoint out_e(new _endpoint);
	out_e->tag = tag_dictionary;
	out_e->size = n;

	switch(e->tag) {
		case tag_tensor: {
			int64_t sz_per_batch = eval_game_constants(e->dims);
			if(is_board(e->dims)) {
				out_t = convolution->forward(e->values.reshape({n, sz_per_batch/CELLS, BOARD_WIDTH, BOARD_HEIGHT}));
			} else {
				out_t = linear
					->forward(e->values.reshape({n, sz_per_batch}))
					.reshape({n, FILTERS, 1, 1})
					.expand({n, FILTERS, BOARD_WIDTH, BOARD_HEIGHT});
			}
			break;
		}
		case tag_vector: {
			sp_endpoint addends(new _endpoint);
			addends->tag = tag_vector;
			addends->size = n;
			addends->dims.push_back(~vec.size());
			addends->vec.reserve(vec.size());

			out_t = torch::zeros({n, FILTERS, BOARD_WIDTH, BOARD_HEIGHT}, GPU_FLOAT);
			for(int i = 0; i < vec.size(); ++i) {
				sp_endpoint addend = vec[i]->activations(e->vec[i]);
				addends->vec.push_back(addend);
				out_t += addend->dict["output"]->values;
			}

			out_e->add_child("addends", addends);
			break;
		}
		case tag_dictionary: {
			out_t = torch::zeros({n, FILTERS, BOARD_WIDTH, BOARD_HEIGHT}, GPU_FLOAT);
			for(auto [nm, child]: dict) {
				assert(nm != "output"); // TODO: handle this more carefully
				sp_endpoint addend = child->activations(e->dict[nm]);
				out_e->add_child(nm, addend);
				out_t += addend->dict["output"]->values;
			}
			break;
		}
		default:
			std::cerr << "Invalid tag " << e->tag << " in EncoderImpl::forward()." << std::endl;
			throw 0;
	}

	// TODO: better dimensions
	out_e->add_child("output", tensor_endpoint(out_t));
	return out_e;
}

sp_endpoint DecoderImpl::forward(const torch::Tensor &t) {
	DebugScope dbg("DecoderImpl::forward", DEFAULT_LAYER_VERBOSITY);

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

sp_endpoint tensor_endpoint(const torch::Tensor &t) {
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

class NetImpl : public torch::nn::Module {
	public:
		NetImpl(sp_structure in, sp_structure out)
			: torch::nn::Module("Nurse Sveta net")
			, enc(in)
			, dec(out)
		{
			DebugScope dbg("NetImpl()", DEFAULT_CONSTRUCTOR_VERBOSITY);

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
			DebugScope dbg("NetImpl::forward");
			torch::Tensor t = enc->forward(in);
			for(int i = 0; i < RESIDUAL_BLOCKS; i++) t = residuals[i]->forward(t);
			return dec->forward(t);
		}

		sp_endpoint activations(sp_endpoint in) {
			DebugScope dbg("NetImpl::activations");

			sp_endpoint res(new _endpoint), out(new _endpoint);
			res->tag = tag_vector;
			out->tag = tag_dictionary;
			res->size = in->size;
			out->size = in->size;
			res->dims.push_back(~residuals.size());
			res->vec.reserve(residuals.size());

			out->add_child("encoder", enc->activations(in));
			torch::Tensor t = out->dict["encoder"]->dict["output"]->values;
			for(auto residual: residuals) {
				sp_endpoint tmp = residual->activations(t);
				res->vec.push_back(tmp);
				t = tmp->dict["output"]->values;
			}
			out->add_child("residuals", res);
			out->add_child("decoder", dec->activations(t));

			return out;
		}

		sp_endpoint weights() {
			DebugScope dbg("NetImpl::weights");

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
TORCH_MODULE(Net);

torch::Tensor leaf_loss(leaf_type loss_ty, const torch::Tensor &net_output, const torch::Tensor &ground_truth, const torch::Tensor &mask) {
	DebugScope dbg("leaf_loss");

	if(ground_truth.sizes() != net_output.sizes() || (mask.defined() && ground_truth.sizes() != mask.sizes())) {
		std::cerr << "Mismatched tensor shapes in leaf_loss" << std::endl;
		std::cerr << "Ground truth: " << TensorSketch(ground_truth) << std::endl;
		std::cerr << "Net output: " << TensorSketch(net_output) << std::endl;
		if(mask.defined()) std::cerr << "Mask: " << TensorSketch(mask) << std::endl;
		throw 0;
	}

	if(CHECK_MASK_VALIDITY && mask.defined() && !ground_truth.equal(ground_truth * mask)) {
		std::cerr << "Invalid mask found in leaf_loss: some masked values were nonzero." << std::endl;
		std::cerr << "Values: " << ground_truth << std::endl;
		std::cerr << "Mask: " << mask << std::endl;
		throw 0;
	}

	switch(loss_ty) {
		case type_unit: [[fallthrough]];
		case type_positive:
			return mask.defined() ?
				(ground_truth - net_output).square() :
				(ground_truth - net_output * mask).square();
		case type_categorical: {
			std::vector<int64_t> sum_dimensions; sum_dimensions.reserve(ground_truth.dim() - 1);
			for(int i = 1; i < ground_truth.dim(); ++i) sum_dimensions.push_back(i);
			torch::Tensor normalization =
				mask.defined() ?
				net_output.sum(sum_dimensions, true) :
				(net_output * mask).sum(sum_dimensions, true);

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
			if(mask.defined()) first_cross_entropy_term *= mask;
			return first_cross_entropy_term - ground_truth * clamped_output.log();
			}
		default:
			std::cerr << "Invalid loss type " << loss_ty << " in leaf_loss." << std::endl;
			throw 0;
	}
}

torch::Tensor loss(sp_structure shape, sp_endpoint net_output, sp_endpoint ground_truth) {
	DebugScope dbg("loss(shape, net_output, ground_truth)");
	if(shape->tag != net_output->tag || shape->tag != ground_truth->tag) {
		std::cerr << "Tag mismatch in loss." << std::endl;
		std::cerr << "shape:        " << shape       ->tag << std::endl;
		std::cerr << "net output:   " << net_output  ->tag << std::endl;
		std::cerr << "ground truth: " << ground_truth->tag << std::endl;
		throw 0;
	}

	torch::Tensor sum = torch::zeros({}, GPU_FLOAT);
	switch(shape->tag) {
		case tag_vector:
			for(int i = 0; i < ground_truth->vec.size(); ++i)
				sum += loss(shape->vec, net_output->vec[i], ground_truth->vec[i]);
			break;
		case tag_dictionary:
			for(auto kv: shape->dict)
				sum += loss(kv.second, net_output->dict[kv.first], ground_truth->dict[kv.first]);
			break;
		case tag_tensor:
			sum = leaf_loss(shape->ty, net_output->values, ground_truth->values, ground_truth->mask).sum() / ground_truth->values.size(0);
			break;
		default:
			std::cerr << "Invalid structure tag " << shape->tag << " in loss." << std::endl;
			throw 0;
	}
	return sum;
}

// This is a fused version of:
//     endpoint_sum(loss_components(shape, scaling, net_output, ground_truth))
// Perhaps it would be better to break it up into those two, at the cost of a
// few extra memory allocations/deallocations, just to separate concerns and
// reduce code duplication?
torch::Tensor loss(sp_structure shape, sp_endpoint scaling, sp_endpoint net_output, sp_endpoint ground_truth) {
	DebugScope dbg("loss(shape, scaling, net_output, ground_truth)");
	if(scaling->tag == tag_tensor) {
		if(scaling->mask.defined()) {
			std::cerr << "Masking the loss doesn't really make sense, just set the scaling for that component to 0 instead." << std::endl;
			throw 0;
		}
		if(scaling->values.dim() != 1) {
			std::cerr << "Scaling tensor output losses differently by position is not (yet) implemented." << std::endl;
			throw 0;
		}
		if(scaling->values.size(0) != 1) {
			std::cerr << "Scaling losses differently per training example is not (yet) implemented." << std::endl;
			throw 0;
		}
		return scaling->values[0] * loss(shape, net_output, ground_truth);
	}

	if(shape->tag != scaling->tag || shape->tag != net_output->tag || shape->tag != ground_truth->tag) {
		std::cerr << "Tag mismatch in loss." << std::endl;
		std::cerr << "shape:        " << shape       ->tag << std::endl;
		std::cerr << "scaling:      " << scaling     ->tag << std::endl;
		std::cerr << "net output:   " << net_output  ->tag << std::endl;
		std::cerr << "ground truth: " << ground_truth->tag << std::endl;
		throw 0;
	}

	torch::Tensor sum = torch::zeros({}, GPU_FLOAT);
	switch(shape->tag) {
		case tag_vector:
			for(int i = 0; i < ground_truth->vec.size(); ++i)
				sum += loss(shape->vec, scaling->vec[i], net_output->vec[i], ground_truth->vec[i]);
			break;
		case tag_dictionary:
			for(auto kv: shape->dict)
				sum += loss(kv.second, scaling->dict[kv.first], net_output->dict[kv.first], ground_truth->dict[kv.first]);
			break;
		default:
			std::cerr << "Invalid structure tag " << shape->tag << " in loss." << std::endl;
			throw 0;
	}
	return sum;
}

sp_endpoint loss_components(sp_structure shape, sp_endpoint scaling, sp_endpoint net_output, sp_endpoint ground_truth) {
	if((shape->tag != scaling->tag && scaling->tag != tag_tensor) || shape->tag != net_output->tag || shape->tag != ground_truth->tag) {
		std::cerr << "Tag mismatch in loss_components." << std::endl;
		std::cerr << "shape:        " << shape       ->tag << std::endl;
		std::cerr << "scaling:      " << scaling     ->tag << std::endl;
		std::cerr << "net output:   " << net_output  ->tag << std::endl;
		std::cerr << "ground truth: " << ground_truth->tag << std::endl;
		throw 0;
	}

	sp_endpoint out(new _endpoint);
	out->tag = scaling->tag;
	out->size = 1;

	switch(scaling->tag) {
		case tag_tensor:
			if(scaling->mask.defined()) {
				std::cerr << "Masking the loss doesn't really make sense, just set the scaling for that component to 0 instead." << std::endl;
				throw 0;
			}
			if(scaling->values.dim() != 1) {
				std::cerr << "Scaling tensor output losses differently by position is not (yet) implemented." << std::endl;
				throw 0;
			}
			out->values = scaling->values * loss(shape, net_output, ground_truth);
			break;
		case tag_vector:
			out->vec.reserve(ground_truth->vec.size());
			for(int i = 0; i < ground_truth->vec.size(); ++i)
				out->vec.push_back(loss_components(shape->vec, scaling->vec[i], net_output->vec[i], ground_truth->vec[i]));
			break;
		case tag_dictionary:
			for(auto kv: shape->dict)
				out->dict[kv.first] = loss_components(kv.second, scaling->dict[kv.first], net_output->dict[kv.first], ground_truth->dict[kv.first]);
			break;
		default:
			std::cerr << "Invalid structure tag " << shape->tag << " in loss_components." << std::endl;
			throw 0;
	}

	return out;
}

float train_net(Net &net, torch::optim::SGD &optim, sp_endpoint scaling, sp_endpoint training_example) {
	if(training_example->tag != tag_dictionary) {
		std::cerr << "train_net expects a dictionary with keys \"input\" and \"ground truth\", but got an endpoint with tag " << training_example->tag << std::endl;
		throw 0;
	}

	optim.zero_grad();
	torch::Tensor sum = loss(net->dec->shape, scaling, net->forward(training_example->dict["input"]), training_example->dict["ground truth"]);
	sum.backward();
	optim.step();
	return sum.item<float>();
}

extern "C" {
	void sample_net(structure *in, structure *out, Net **net_out, torch::optim::SGD **optim_out);
	void load_net(char *path, structure *in, structure *out, Net **net_out, torch::optim::SGD **optim_out);
	void discard_net(Net *net);
	void save_net(Net *net, torch::optim::SGD *optim, char *path);
	endpoint *evaluate_net(Net *net, endpoint *in);
	endpoint *loss_components(Net *net, endpoint *scaling, endpoint *net_output, endpoint *ground_truth);
	endpoint *net_activations(Net *net, endpoint *in);
	float train_net(Net *net, torch::optim::SGD *optim, endpoint *scaling, endpoint *training_example);
	endpoint *net_weights(Net *net);
	void discard_optimizer(torch::optim::SGD *optim);
}

void sample_net(structure *in, structure *out, Net **net_out, torch::optim::SGD **optim_out) {
	DebugScope dbg("sample_net", DEFAULT_CONSTRUCTOR_VERBOSITY);
	*net_out = new Net(in->ref, out->ref);
	Net &net = **net_out;
	if(!net->is_training()) {
		std::cerr << "The impossible happened: a freshly created net was not in training mode." << std::endl;
		throw 0;
	}
	// TODO: allow setting SGD parameters like momentum, learning rate, etc.
	*optim_out = new torch::optim::SGD(net->parameters(), INITIAL_LEARNING_RATE);
}

void load_net(char *path, structure *in, structure *out, Net **net_out, torch::optim::SGD **optim_out) {
	DebugScope dbg("load_net", DEFAULT_SERIALIZATION_VERBOSITY);
	torch::serialize::InputArchive archive;
	archive.load_from(path);

	*net_out = new Net(in->ref, out->ref);
	Net &net = **net_out;
	net->load(archive);
	if(optim_out == nullptr) {
		net->train(false);
	} else {
		if(!net->is_training())
			std::cerr << "WARNING: loaded a net that wasn't in training mode" << std::endl;
		// TODO: load SGD parameters, they aren't saved with the SGD state FFS
		*optim_out = new torch::optim::SGD(net->parameters(), INITIAL_LEARNING_RATE);
		(*optim_out)->load(archive);
	}
}

void discard_net(Net *net) { delete net; }

// TODO: perhaps it would be nice to save the encoder and decoder shapes we've
// recorded, then either load them or validate them against the user-supplied
// shapes when loading or something
void save_net(Net *net, torch::optim::SGD *optim, char *path) {
	DebugScope dbg("save_net", DEFAULT_SERIALIZATION_VERBOSITY);
	torch::serialize::OutputArchive archive;
	(**net).save(archive);
	optim->save(archive);
	archive.save_to(path);
}

endpoint *evaluate_net(Net *net, endpoint *in) {
	DebugScope dbg("evaluate_net");
	torch::NoGradGuard g;
	if((*net)->is_training())
		std::cerr << "WARNING: evaluating a net while in training mode" << std::endl;
	return new endpoint((*net)->forward(in->ref));
}

endpoint *loss_components(Net *net_, endpoint *scaling, endpoint *net_output, endpoint *ground_truth) {
	DebugScope dbg("loss_components");
	torch::NoGradGuard g;
	Net &net = *net_;
	bool was_training = net->is_training();
	net->train(false);
	sp_endpoint out = loss_components(net->dec->shape, scaling->ref, net_output->ref, ground_truth->ref);
	net->train(was_training);
	return new endpoint(out);
}

endpoint *net_activations(Net *net_, endpoint *in) {
	DebugScope dbg("net_activations");
	torch::NoGradGuard g;
	Net &net = *net_;
	bool was_training = net->is_training();
	net->train(false);
	sp_endpoint out = net->activations(in->ref);
	net->train(was_training);
	return new endpoint(out);
}

float train_net(Net *net, torch::optim::SGD *optim, endpoint *scaling, endpoint *training_example) {
	DebugScope dbg("train_net");
	if(!(*net)->is_training())
		std::cerr << "WARNING: training a net while in evaluation mode" << std::endl;
	return train_net(*net, *optim, scaling->ref, training_example->ref);
}

endpoint *net_weights(Net *net) {
	DebugScope dbg("net_weights");
	return new endpoint((*net)->weights());
}

void discard_optimizer(torch::optim::SGD *optim) {
	DebugScope dbg("discard_optimizer", DEFAULT_CONSTRUCTOR_VERBOSITY);
	delete optim;
}
