#include <torch/torch.h>

const int64_t BOARD_WIDTH = 8;
const int64_t BOARD_HEIGHT = 16;
const int64_t LOOKAHEAD_SIZE = 6;
const int64_t CELL_SIZE = 7;
const int64_t SCALARS = 6; // frames, log(frames), sqrt(frames), starting viruses, log(starting viruses), 1/sqrt(starting viruses)
const int64_t ROTATIONS = 4;
const int64_t ORIENTATIONS = 2;
const int64_t COLORS = 3;

const int64_t CELLS = BOARD_WIDTH * BOARD_HEIGHT;
const int64_t PILLCONTENTS = ORIENTATIONS*COLORS*COLORS;

// priors, valuation, fall time, occupied, virus kills, future pill placements, clear locations, clear pills
const int64_t OUTPUT_LAYERS = ROTATIONS + 1 + 1 + 1 + 1 + PILLCONTENTS + 1 + 1;
const int64_t OUTPUT_TYPES = 8;

const int64_t FILTERS = 32;
const int64_t RESIDUAL_BLOCKS = 20;
const double LEAKAGE = 0.01;

const int64_t BODY_SIZE = FILTERS * CELLS;

// TODO: A0 started its LR off at 0.2 (!)
const double INITIAL_LEARNING_RATE = 1e-6;

struct TensorSketch {
	torch::Device dev;
	torch::ScalarType ty;
	std::vector<int> dims;
	bool grad;

	TensorSketch(torch::Tensor t)
		: dev(t.device()), ty(t.scalar_type()), grad(t.requires_grad())
	{
		for(int i = 0; i < t.dim(); i++) dims.push_back(t.size(i));
	}
};

std::ostream &operator<<(std::ostream &o, const TensorSketch sketch) {
	o << sketch.ty << "[";
	if(sketch.dims.size() > 0) {
		o << sketch.dims[0];
		for(int i = 1; i < sketch.dims.size(); i++)
			o << ", " << sketch.dims[i];
	}
	return o << "]@" << sketch.dev << (sketch.grad?"+":"-");
}

struct NetInput {
	// boards: [n, 8, 16]
	// lookaheads: [n, 6]
	// scalars: [n, 4]
	torch::Tensor boards, lookaheads, scalars;

	NetInput() {}

	NetInput(int n, char *boards_data, char *lookaheads_data, double *scalars_data) {
		auto  u8Options = torch::TensorOptions().dtype(torch::kU8);
		auto f64Options = torch::TensorOptions().dtype(torch::kF64);
		boards     = torch::from_blob(    boards_data, {n, CELL_SIZE, BOARD_WIDTH, BOARD_HEIGHT}, [](void *v){},  u8Options);
		lookaheads = torch::from_blob(lookaheads_data, {n, LOOKAHEAD_SIZE}                      , [](void *v){},  u8Options);
		scalars    = torch::from_blob(   scalars_data, {n, SCALARS}                             , [](void *v){}, f64Options);
	}

	NetInput(int n) {
		auto  u8Options = torch::TensorOptions().dtype(torch::kU8);
		auto f64Options = torch::TensorOptions().dtype(torch::kF64);
		boards     = torch::empty({n, CELL_SIZE, BOARD_WIDTH, BOARD_HEIGHT},  u8Options);
		lookaheads = torch::empty({n, LOOKAHEAD_SIZE}                      ,  u8Options);
		scalars    = torch::empty({n, SCALARS}                             , f64Options);
	}

	void to_gpu() {
		boards     = boards    .to(torch::kCUDA);
		lookaheads = lookaheads.to(torch::kCUDA);
		scalars    = scalars   .to(torch::kCUDA);
		boards     = boards    .to(torch::kF64);
		lookaheads = lookaheads.to(torch::kF64);
	}

	void save(torch::serialize::OutputArchive &archive) const {
		archive.write("boards"    , boards    , true);
		archive.write("lookaheads", lookaheads, true);
		archive.write("scalars"   , scalars   , true);
	}

	void load(torch::serialize::InputArchive &archive) {
		archive.read("boards"    , boards    , true);
		archive.read("lookaheads", lookaheads, true);
		archive.read("scalars"   , scalars   , true);
	}

	std::ostream &dump(std::ostream &o) const {
		return o
			<<     "boards: " << boards     << std::endl
			<< "lookaheads: " << lookaheads << std::endl
			<<    "scalars: " << scalars    << std::endl
			;
	}

	std::ostream &sketch(std::ostream &o) const {
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
	// valuation: [n, 1]
	// fall_time: [n, 1]
	// occupied: [n, 8, 16]
	// virus_kills: [n, 8, 16]
	// wishlist: [n, 2, 3, 3, 8, 16]
	// clear_location: [n, 8, 16]
	// clear_pill: [n, 2, 3, 3]
	torch::Tensor priors, valuation, fall_time, occupied, virus_kills, wishlist, clear_location, clear_pill;

	NetOutput() {}

	NetOutput(int n, double *priors_data, double *valuation_data, unsigned char *fall_time_data, char *occupied_data, double *virus_kills_data, double *wishlist_data, double *clear_location_data, double *clear_pill_data) {
		auto f64Opts = torch::TensorOptions().dtype(torch::kF64);
		auto  u8Opts = torch::TensorOptions().dtype(torch::kU8 );

		priors         = torch::from_blob(        priors_data, {n, ROTATIONS, BOARD_WIDTH, BOARD_HEIGHT                   }, [](void *) {}, f64Opts);
		valuation      = torch::from_blob(     valuation_data, {n, 1                                                      }, [](void *) {}, f64Opts);
		fall_time      = torch::from_blob(     fall_time_data, {n, 1                                                      }, [](void *) {},  u8Opts);
		occupied       = torch::from_blob(      occupied_data, {n, BOARD_WIDTH, BOARD_HEIGHT                              }, [](void *) {},  u8Opts);
		virus_kills    = torch::from_blob(   virus_kills_data, {n, BOARD_WIDTH, BOARD_HEIGHT                              }, [](void *) {}, f64Opts);
		wishlist       = torch::from_blob(      wishlist_data, {n, ORIENTATIONS, COLORS, COLORS, BOARD_WIDTH, BOARD_HEIGHT}, [](void *) {}, f64Opts);
		clear_location = torch::from_blob(clear_location_data, {n, BOARD_WIDTH, BOARD_HEIGHT                              }, [](void *) {}, f64Opts);
		clear_pill     = torch::from_blob(    clear_pill_data, {n, ORIENTATIONS, COLORS, COLORS                           }, [](void *) {}, f64Opts);
	}

	NetOutput(int n) {
		auto f64Opts = torch::TensorOptions().dtype(torch::kF64);
		auto  u8Opts = torch::TensorOptions().dtype(torch::kU8 );
		priors         = torch::empty({n, ROTATIONS, BOARD_WIDTH, BOARD_HEIGHT                   }, f64Opts);
		valuation      = torch::empty({n, 1                                                      }, f64Opts);
		fall_time      = torch::empty({n, 1                                                      },  u8Opts);
		occupied       = torch::empty({n, BOARD_WIDTH, BOARD_HEIGHT                              },  u8Opts);
		virus_kills    = torch::empty({n, BOARD_WIDTH, BOARD_HEIGHT                              }, f64Opts);
		wishlist       = torch::empty({n, ORIENTATIONS, COLORS, COLORS, BOARD_WIDTH, BOARD_HEIGHT}, f64Opts);
		clear_location = torch::empty({n, BOARD_WIDTH, BOARD_HEIGHT                              }, f64Opts);
		clear_pill     = torch::empty({n, ORIENTATIONS, COLORS, COLORS                           }, f64Opts);
	}

	void to_gpu() {
		priors         = priors        .to(torch::kCUDA);
		valuation      = valuation     .to(torch::kCUDA);
		fall_time      = fall_time     .to(torch::kCUDA);
		occupied       = occupied      .to(torch::kCUDA);
		virus_kills    = virus_kills   .to(torch::kCUDA);
		wishlist       = wishlist      .to(torch::kCUDA);
		clear_location = clear_location.to(torch::kCUDA);
		clear_pill     = clear_pill    .to(torch::kCUDA);
		fall_time      = fall_time     .to(torch::kF64);
		occupied       = occupied      .to(torch::kF64);
	}

	void to_cpu() {
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

	Batch() {}

	Batch(int n): in(n), out(n) {
		auto charOptions = torch::TensorOptions().dtype(torch::kU8);
		reachable = torch::empty({n, ROTATIONS, BOARD_WIDTH, BOARD_HEIGHT}, charOptions);
	}

	Batch(int n,
		char *reachable_data,
		double *priors_data, double *valuation_data, unsigned char *fall_time_data, char *occupied_data, double *virus_kills_data, double *wishlist_data, double *clear_location_data, double *clear_pill_data,
		char *boards_data, char *lookaheads_data, double *scalars_data
		)
		: in(n, boards_data, lookaheads_data, scalars_data)
		, out(n, priors_data, valuation_data, fall_time_data, occupied_data, virus_kills_data, wishlist_data, clear_location_data, clear_pill_data) {
		auto charOptions = torch::TensorOptions().dtype(torch::kU8);
		reachable = torch::from_blob(reachable_data, {n, ROTATIONS, BOARD_WIDTH, BOARD_HEIGHT}, [](void *){}, charOptions);
	}

	void to_gpu() {
		in.to_gpu();
		out.to_gpu();
		reachable = reachable.to(torch::kCUDA);
		reachable = reachable.to(torch::kF64);
	}

	void save(torch::serialize::OutputArchive &archive) {
		in.save(archive);
		out.save(archive);
		archive.write("reachable", reachable, true);
	}

	void load(torch::serialize::InputArchive &archive) {
		in.load(archive);
		out.load(archive);
		archive.read("reachable", reachable, true);
	}

	std::ostream &dump(std::ostream &o) const {
		return o
			<< in
			<< out
			<< "reachable: " << reachable << std::endl;
	}

	std::ostream &sketch(std::ostream &o) const {
		in.sketch(o) << ", ";
		out.sketch(o) << ", ";
		return o << "reachable: " << TensorSketch(reachable);
	}
};

std::ostream &operator<<(std::ostream &o, const Batch &batch) { return batch.dump(o); }

class Conv2dReLUInitImpl : public torch::nn::Module {
	public:
		Conv2dReLUInitImpl(int64_t iChans, int64_t oChans, double leakage, int64_t padding=1, int64_t k=3)
			: torch::nn::Module("Conv2d (custom initialization)")
			, conv(torch::nn::Conv2d(torch::nn::Conv2dOptions(iChans, oChans, k).padding(padding)))
		{
			auto opts = torch::TensorOptions().dtype(torch::kFloat64).requires_grad(true);
			// scaling factor is from Delving Deep into Rectifiers
			auto scaling = std::sqrt(2 / ((1 + leakage*leakage) * k*k*iChans));
			torch::autograd::GradMode::set_enabled(false);
			conv->weight.normal_(0, scaling);
			conv->bias.normal_(0, 1);
			torch::autograd::GradMode::set_enabled(true);
			register_module("conv", conv);
		}

		torch::Tensor forward(const torch::Tensor &in) { return conv->forward(in); }

		torch::nn::Conv2d conv;
};
TORCH_MODULE(Conv2dReLUInit);

class ResidualImpl : public torch::nn::Module {
	public:
		ResidualImpl(int64_t chans, double leakage, int64_t padding=1, int64_t k=3)
			: torch::nn::Module("residual block")
			, conv0(chans, chans, leakage, padding, k)
			, conv1(chans, chans, leakage, padding, k)
			, norm0(torch::nn::BatchNorm2dOptions(chans))
			, norm1(torch::nn::BatchNorm2dOptions(chans))
			, lrelu(torch::nn::LeakyReLU(torch::nn::LeakyReLUOptions().negative_slope(leakage)))
		{
			register_module("conv0", conv0);
			register_module("conv1", conv1);
			register_module("norm0", norm0);
			register_module("norm1", norm1);
			register_module("lrelu", lrelu);
		}

		torch::Tensor forward(const torch::Tensor &in) {
			return lrelu->forward(in +
				norm1->forward(
				conv1->forward(
				lrelu->forward(
				norm0->forward(
				conv0->forward(
				in))))));
		}

	Conv2dReLUInit conv0, conv1;
	torch::nn::BatchNorm2d norm0, norm1;
	torch::nn::LeakyReLU lrelu;
};
TORCH_MODULE(Residual);

class NetImpl : public torch::nn::Module {
	public:
		NetImpl()
			: torch::nn::Module("Nurse Sveta net")
			, board_convolution(CELL_SIZE, FILTERS, LEAKAGE)
			, lookahead_linear(LOOKAHEAD_SIZE, FILTERS)
			, scalar_linear(SCALARS, FILTERS)
			, input_norm(torch::nn::BatchNorm2dOptions(FILTERS))
			, output_convolution(FILTERS, OUTPUT_LAYERS, LEAKAGE, 0, 1)
			, output_norm(torch::nn::BatchNorm2dOptions(OUTPUT_LAYERS))
			, output_lrelu(torch::nn::LeakyReLUOptions().negative_slope(LEAKAGE))
			{
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
				register_module("output normalization", output_norm);
				register_module("output ReLU", output_lrelu);

				register_output("priors", ROTATIONS);
				register_output("valuation", 1, 1);
				register_output("fall time", 1, 1);
				register_output("occupied", 1);
				register_output("virus kills", 1);
				register_output("wishlist", PILLCONTENTS);
				register_output("clear location", 1);
				register_output("clear pill", 1, PILLCONTENTS);
				if(output_linears.size() != OUTPUT_LAYERS) throw 0;

				to(torch::kCUDA);
				to(torch::kF64);
			}

		NetOutput forward(const NetInput &in);

	private:
		Conv2dReLUInit board_convolution, output_convolution;
		torch::nn::Linear lookahead_linear, scalar_linear;
		torch::nn::BatchNorm2d input_norm, output_norm;
		std::vector<Residual> residuals;
		torch::nn::LeakyReLU output_lrelu;
		std::vector<torch::nn::Linear> output_linears;

		void register_output(std::string name, int64_t count, int64_t size=CELLS) {
			for(int64_t i = 0; i < count; i++) {
				auto layer = torch::nn::Linear(CELLS, size);
				std::stringstream ss;
				ss << name << " output linear #" << i;
				output_linears.push_back(layer);
				register_module(ss.str(), layer);
			}
		}
};
TORCH_MODULE(Net);

NetOutput NetImpl::forward(const NetInput &in) {
	const int64_t n = in.boards.size(0);
	torch::Tensor t = input_norm->forward(
		board_convolution->forward(in.boards) +
		lookahead_linear->forward(in.lookaheads).reshape({n, FILTERS, 1, 1}).expand({n, FILTERS, BOARD_WIDTH, BOARD_HEIGHT}) +
		   scalar_linear->forward(in.scalars   ).reshape({n, FILTERS, 1, 1}).expand({n, FILTERS, BOARD_WIDTH, BOARD_HEIGHT})
		);
	for(int64_t i = 0; i < RESIDUAL_BLOCKS; i++) t = residuals[i]->forward(t);
	t = output_lrelu->forward(output_norm->forward(output_convolution->forward(t)));

	NetOutput out;
	int64_t i = 0;

	auto opts = torch::TensorOptions().dtype(torch::kF64).device(torch::kCUDA);
	out.priors = torch::empty({n, ROTATIONS, BOARD_WIDTH, BOARD_HEIGHT}, opts);
	out.wishlist = torch::empty({n, ORIENTATIONS, COLORS, COLORS, BOARD_WIDTH, BOARD_HEIGHT}, opts);
	std::vector<torch::Tensor> ts;
	for(int j = 0; j < OUTPUT_LAYERS; j++) {
		ts.push_back(t.index({torch::indexing::Slice(), j, "..."}).reshape({n, CELLS}));
	}

	for(int64_t rotation = 0; rotation < ROTATIONS; rotation++) {
		out.priors.index_put_({torch::indexing::Slice(), rotation, "..."}, output_linears[i]->forward(ts[i]).exp().reshape({n, BOARD_WIDTH, BOARD_HEIGHT})); i++;
	}
	out.valuation      = output_linears[i]->forward(ts[i]).sigmoid(); i++;
	out.fall_time      = output_linears[i]->forward(ts[i]); i++;
	out.occupied       = output_linears[i]->forward(ts[i]).sigmoid().reshape({n, BOARD_WIDTH, BOARD_HEIGHT}); i++;
	out.virus_kills    = output_linears[i]->forward(ts[i]).sigmoid().reshape({n, BOARD_WIDTH, BOARD_HEIGHT}); i++;
	for(int64_t orientation = 0; orientation < ORIENTATIONS; orientation++) {
		for(int64_t colorl = 0; colorl < COLORS; colorl++) {
			for(int64_t colorr = 0; colorr < COLORS; colorr++) {
				out.wishlist.index_put_(
					{torch::indexing::Slice(), orientation, colorl, colorr, "..."},
					output_linears[i]->forward(ts[i]).sigmoid().reshape({n, BOARD_WIDTH, BOARD_HEIGHT})
					);
				i++;
			}
		}
	}
	out.clear_location = output_linears[i]->forward(ts[i]).sigmoid().reshape({n, BOARD_WIDTH, BOARD_HEIGHT}); i++;
	out.clear_pill     = output_linears[i]->forward(ts[i]).sigmoid().reshape({n, ORIENTATIONS, COLORS, COLORS}); i++;
	if(i != OUTPUT_LAYERS) throw 0;

	return out;
}

torch::Tensor detailed_loss(Net &net, const Batch &batch) {
	const int n = batch.out.priors.size(0);
	auto net_out = net->forward(batch.in);
	auto opts = torch::TensorOptions().dtype(torch::kF64).device(torch::kCUDA);
	auto loss = torch::zeros({OUTPUT_TYPES}, opts);
	int64_t i = 0;
	// Kullback-Leibler divergence for priors
	auto scaled_priors = net_out.priors / (batch.reachable*net_out.priors).sum({1,2,3}, true);
	// correct_out.priors can have zeros (e.g. everywhere reachable is zero),
	// which leads to -inf's after the log, which leads to nan's in the
	// gradients. I also tried clamping after the log, but the nan's show up
	// even after multiplying by reachable to screen off the effect of the
	// incoming zeros.
	loss.index_put_({i++}, (batch.out.priors * (batch.out.priors.clamp_min(1e-20) / scaled_priors).log() * batch.reachable).sum());
	// cross-entropy loss for valuation
	// getting the evaluation function right is much more important than
	// getting the policy or predictions right, so scale this loss term way up
	auto bs = net_out.valuation.clamp(1e-10, 1-1e-10);
	loss.index_put_({i++}, 1000 * (batch.out.valuation * bs.log() + (1 - batch.out.valuation) * (1 - bs).log()).sum().neg());
	// squared-error loss for everything else
	loss.index_put_({i++}, (batch.out.fall_time      - net_out.fall_time     ).square().sum());
	loss.index_put_({i++}, (batch.out.occupied       - net_out.occupied      ).square().sum());
	loss.index_put_({i++}, (batch.out.virus_kills    - net_out.virus_kills   ).square().sum());
	loss.index_put_({i++}, (batch.out.wishlist       - net_out.wishlist      ).square().sum());
	loss.index_put_({i++}, (batch.out.clear_location - net_out.clear_location).square().sum());
	loss.index_put_({i++}, (batch.out.clear_pill     - net_out.clear_pill    ).square().sum());
	if(i != OUTPUT_TYPES) throw 0;
	loss /= n;
	return loss;
}

void tensorcpy(double *out, torch::Tensor &in) {
	if(in.dtype() != torch::kF64 || in.device() != torch::kCPU) throw 0;
	int64_t len = 1;
	for(int i = 0; i < in.dim(); i++) len *= in.size(i);

	// TODO: is it safe to inline the definition of contig_in, or will that
	// lead to the Tensor being destructed before memcpy finishes?
	auto contig_in = in.contiguous();
	std::memcpy(out, contig_in.data_ptr<double>(), len*sizeof(double));
}

extern "C" {
	Net *sample_net(bool training);
	void save_net(Net *net, torch::optim::SGD *optim, char *path);
	void load_net(char *path, Net **net, torch::optim::SGD **optim);
	void discard_net(Net *net);
	void evaluate_net(Net *net, int n, double *priors, double *valuation, char *boards, char *lookaheads, double *scalars);
	void save_example(char *path, char *reachable, double *priors, double valuation, unsigned char fall_time, char *occupied, double *virus_kills, double *wishlist, double *clear_location, double *clear_pill, char *board, char *lookahead, double *scalars);
	Batch *load_batch(char **path, int n);
	void discard_batch(Batch *batch);
	double train_net(Net *net, torch::optim::SGD *optim, Batch *batch, unsigned long loss_mask);
	void detailed_loss(Net *net, double *out, Batch *batch);
	torch::optim::SGD *connect_optimizer(Net *net);
	void discard_optimizer(torch::optim::SGD *optim);
}

Net *sample_net(bool training) {
	Net *net = new Net();
	// TODO: does this do everything we want? turn off autograd e.g.?
	(**net).train(training);
	return net;
}
void discard_net(Net *net) { delete net; }

void save_net(Net *net, torch::optim::SGD *optim, char *path) {
	torch::serialize::OutputArchive archive;
	(**net).save(archive);
	optim->save(archive);
	archive.save_to(path);
}

void load_net(char *path, Net **netptr, torch::optim::SGD **optimptr) {
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

void evaluate_net(Net *net, int n, double *priors, double *valuation, char *boards, char *lookaheads, double *scalars) {
	torch::NoGradGuard g;
	NetInput in(n, boards, lookaheads, scalars); in.to_gpu();
	NetOutput out = (**net).forward(in);

	out.priors = out.priors.to(torch::kCPU);
	out.valuation = out.valuation.to(torch::kCPU);
	tensorcpy(priors,    out.priors);
	tensorcpy(valuation, out.valuation);
}

void save_example(char *path, char *reachable,
	double *priors, double valuation, unsigned char fall_time, char *occupied, double *virus_kills, double *wishlist, double *clear_location, double *clear_pill,
	char *board, char *lookahead, double *scalars) {
	Batch batch(1, reachable, priors, &valuation, &fall_time, occupied, virus_kills, wishlist, clear_location, clear_pill, board, lookahead, scalars);
	torch::serialize::OutputArchive archive;
	batch.save(archive);
	archive.save_to(path);
}

Batch *load_batch(char **path, int n) {
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

void discard_batch(Batch *batch) { delete batch; }

double train_net(Net *net, torch::optim::SGD *optim, Batch *batch, unsigned long loss_mask) {
	optim->zero_grad();
	auto losses = detailed_loss(*net, *batch);

	double loss_mask_array[OUTPUT_TYPES];
	for(int i = 0; i < OUTPUT_TYPES; i++) {
		loss_mask_array[i] = (loss_mask & (1 << i))?1:0;
	}
	auto loss_mask_tensor = torch::from_blob(loss_mask_array, {OUTPUT_TYPES}, [](void *v){}, torch::TensorOptions().dtype(torch::kF64)).to(torch::kCUDA);
	auto loss = (losses * loss_mask_tensor).sum();

	loss.sum().backward();
	optim->step();
	loss = loss.to(torch::kCPU);
	return *loss.data_ptr<double>();
}

void detailed_loss(Net *net, double *out, Batch *batch) {
	torch::NoGradGuard g;
	bool was_training = (**net).is_training();
	(**net).train(false); // don't want to accidentally teach our BatchNorm layers about our test vectors...
	auto loss = detailed_loss(*net, *batch).to(torch::kCPU);
	tensorcpy(out, loss);
	(**net).train(was_training);
}

torch::optim::SGD *connect_optimizer(Net *net) {
	// TODO: allow setting SGD parameters like momentum, learning rate, etc.
	return new torch::optim::SGD((**net).parameters(), INITIAL_LEARNING_RATE);
}

void discard_optimizer(torch::optim::SGD *optim) { delete optim; }
