#include <torch/torch.h>

const int64_t BOARD_WIDTH = 8;
const int64_t BOARD_HEIGHT = 16;
const int64_t LOOKAHEAD_SIZE = 6;
const int64_t CELL_SIZE = 7;
const int64_t NUM_SCALARS = 6; // frames, log(frames), sqrt(frames), starting viruses, log(starting viruses), 1/sqrt(starting viruses)
const int64_t NUM_BERNOULLIS = 1;
const int64_t NUM_ROTATIONS = 4;

const int64_t NUM_CELLS = BOARD_WIDTH * BOARD_HEIGHT;

const int64_t FILTER_COUNT = 32;
const int64_t RESIDUAL_BLOCK_COUNT = 20;
const double LEAKAGE = 0.01;

const int64_t BODY_SIZE = FILTER_COUNT * NUM_CELLS;

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
		scalars    = torch::from_blob(   scalars_data, {n, NUM_SCALARS}                         , [](void *v){}, f64Options);
	}

	NetInput(int n) {
		auto  u8Options = torch::TensorOptions().dtype(torch::kU8);
		auto f64Options = torch::TensorOptions().dtype(torch::kF64);
		boards     = torch::empty({n, CELL_SIZE, BOARD_WIDTH, BOARD_HEIGHT},  u8Options);
		lookaheads = torch::empty({n, LOOKAHEAD_SIZE}                      ,  u8Options);
		scalars    = torch::empty({n, NUM_SCALARS}                         , f64Options);
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
	// bernoullis: [n, 1]
	torch::Tensor priors, bernoullis;

	NetOutput() {}

	NetOutput(int n, double *priors_data, double *bernoullis_data) {
		auto opts = torch::TensorOptions().dtype(torch::kF64);
		priors     = torch::from_blob(    priors_data, {n, NUM_ROTATIONS, BOARD_WIDTH, BOARD_HEIGHT}, [](void *){}, opts);
		bernoullis = torch::from_blob(bernoullis_data, {n, NUM_BERNOULLIS}                          , [](void *){}, opts);
	}

	NetOutput(int n) {
		auto opts = torch::TensorOptions().dtype(torch::kF64);
		priors     = torch::empty({n, NUM_ROTATIONS, BOARD_WIDTH, BOARD_HEIGHT}, opts);
		bernoullis = torch::empty({n, NUM_BERNOULLIS}                          , opts);
	}

	void to_gpu() {
		priors     = priors    .to(torch::kCUDA);
		bernoullis = bernoullis.to(torch::kCUDA);
	}

	void to_cpu() {
		priors     = priors    .to(torch::kCPU);
		bernoullis = bernoullis.to(torch::kCPU);
	}

	void save(torch::serialize::OutputArchive &archive) const {
		archive.write("priors"    , priors    , true);
		archive.write("bernoullis", bernoullis, true);
	}

	void load(torch::serialize::InputArchive &archive) {
		archive.read("priors"    , priors    , true);
		archive.read("bernoullis", bernoullis, true);
	}

	std::ostream &dump(std::ostream &o) const {
		return o
			<<     "priors: " << priors     << std::endl
			<< "bernoullis: " << bernoullis << std::endl
			;
	}

	std::ostream &sketch(std::ostream &o) const {
		return o
			<<     "priors: " << TensorSketch(priors    ) << ", "
			<< "bernoullis: " << TensorSketch(bernoullis) << ", "
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
		reachable = torch::empty({n, NUM_ROTATIONS, BOARD_WIDTH, BOARD_HEIGHT}, charOptions);
	}

	Batch(int n, double *priors_data, char *reachable_data, double *bernoullis_data, char *boards_data, char *lookaheads_data, double *scalars_data)
		: in(n, boards_data, lookaheads_data, scalars_data)
		, out(n, priors_data, bernoullis_data) {
		auto charOptions = torch::TensorOptions().dtype(torch::kU8);
		reachable = torch::from_blob(reachable_data, {n, NUM_ROTATIONS, BOARD_WIDTH, BOARD_HEIGHT}, [](void *){}, charOptions);
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

	std::ostream &dump(std::ostream &o) {
		return o
			<< in
			<< out
			<< "reachable: " << reachable << std::endl;
	}

	std::ostream &sketch(std::ostream &o) {
		in.sketch(o) << ", ";
		out.sketch(o) << ", ";
		return o << "reachable: " << TensorSketch(reachable);
	}
};

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
			, board_convolution(CELL_SIZE, FILTER_COUNT, LEAKAGE)
			, lookahead_linear(LOOKAHEAD_SIZE, FILTER_COUNT)
			, scalar_linear(NUM_SCALARS, FILTER_COUNT)
			, input_norm(torch::nn::BatchNorm2dOptions(FILTER_COUNT))
			, bernoulli_linear(BODY_SIZE, NUM_BERNOULLIS)
			, priors_convolution(FILTER_COUNT, NUM_ROTATIONS, LEAKAGE)
			{
				for(int64_t i = 0; i < RESIDUAL_BLOCK_COUNT; i++)
					residuals.push_back(Residual(FILTER_COUNT, LEAKAGE));

				register_module("initial board convolution", board_convolution);
				register_module("fully-connected feed for lookaheads", lookahead_linear);
				register_module("fully-connected feed for game history statistics", scalar_linear);
				register_module("input normalization", input_norm);

				for(int64_t i = 0; i < RESIDUAL_BLOCK_COUNT; i++) {
					std::stringstream ss;
					ss << "main body residual #" << i;
					register_module(ss.str(), residuals[i]);
				}
				register_module("Bernoulli parameter outputs", bernoulli_linear);
				register_module("final priors convolution", priors_convolution);

				to(torch::kCUDA);
				to(torch::kF64);
			}

		NetOutput forward(const NetInput &in);

	private:
		Conv2dReLUInit board_convolution, priors_convolution;
		torch::nn::Linear lookahead_linear, bernoulli_linear, scalar_linear;
		torch::nn::BatchNorm2d input_norm;
		std::vector<Residual> residuals;
};
TORCH_MODULE(Net);

NetOutput NetImpl::forward(const NetInput &in) {
	const int64_t n = in.boards.size(0);
	torch::Tensor t = input_norm->forward(
		board_convolution->forward(in.boards) +
		lookahead_linear->forward(in.lookaheads).reshape({n, FILTER_COUNT, 1, 1}).expand({n, FILTER_COUNT, BOARD_WIDTH, BOARD_HEIGHT}) +
		scalar_linear->forward(in.scalars).reshape({n, FILTER_COUNT, 1, 1}).expand({n, FILTER_COUNT, BOARD_WIDTH, BOARD_HEIGHT})
		);
	for(int64_t i = 0; i < RESIDUAL_BLOCK_COUNT; i++) t = residuals[i]->forward(t);

	NetOutput out;
	out.priors = priors_convolution->forward(t).exp();
	t = t.reshape({n, BODY_SIZE});
	out.bernoullis = bernoulli_linear->forward(t).sigmoid();

	return out;
}

torch::Tensor detailed_loss(Net &net, const Batch &batch) {
	const int n = batch.out.priors.size(0);
	auto net_out = net->forward(batch.in);
	auto opts = torch::TensorOptions().dtype(torch::kF64).device(torch::kCUDA);
	auto loss = torch::zeros({2}, opts);
	// Kullback-Leibler divergence for priors
	auto scaled_priors = net_out.priors / (batch.reachable*net_out.priors).sum({1,2,3}, true);
	// correct_out.priors can have zeros (e.g. everywhere reachable is zero),
	// which leads to -inf's after the log, which leads to nan's in the
	// gradients. I also tried clamping after the log, but the nan's show up
	// even after multiplying by reachable to screen off the effect of the
	// incoming zeros.
	loss.index_put_({0}, (batch.out.priors * (batch.out.priors.clamp_min(1e-20) / scaled_priors).log() * batch.reachable).sum());
	// cross-entropy loss for bernoullis
	// getting the evaluation function right is much more important than
	// getting the policy write, so scale this loss term up a lot
	auto bs = net_out.bernoullis.clamp(1e-10, 1-1e-10);
	loss.index_put_({1}, 1000 * (batch.out.bernoullis * bs.log() + (1 - batch.out.bernoullis) * (1 - bs).log()).sum().neg());
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
	void evaluate_net(Net *net, int n, double *priors, double *bernoullis, char *boards, char *lookaheads, double *scalars);
	void save_example(char *path, double *priors, char *reachable, double *bernoullis, char *board, char *lookahead, double *scalars);
	Batch *load_batch(char **path, int n);
	void discard_batch(Batch *batch);
	double train_net(Net *net, torch::optim::SGD *optim, Batch *batch);
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
		*optimptr = new torch::optim::SGD(net->parameters(), 0.1);
		(**optimptr).load(archive);
	}
}

// priors: [n, 4, 8, 16]
// bernoullis: [n, 1]
// boards: [n, 7, 8, 16]
// lookaheads: [n, 6]
// scalars: [n, 4]
void evaluate_net(Net *net, int n, double *priors, double *bernoullis, char *boards, char *lookaheads, double *scalars) {
	torch::NoGradGuard g;
	NetInput in(n, boards, lookaheads, scalars); in.to_gpu();
	NetOutput out = (**net).forward(in); out.to_cpu();

	tensorcpy(priors,     out.priors    );
	tensorcpy(bernoullis, out.bernoullis);
}

// priors: [4, 8, 16]
// reachable: [4, 8, 16]
// bernoullis: [1]
// scalars: [3]
// board: [7, 8, 16]
// lookahead: [6]
void save_example(char *path, double *priors, char *reachable, double *bernoullis, char *board, char *lookahead, double *scalars) {
	Batch batch(1, priors, reachable, bernoullis, board, lookahead, scalars);
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
		batch->in .boards    .index_put_({i, "..."}, example.in .boards    );
		batch->in .lookaheads.index_put_({i, "..."}, example.in .lookaheads);
		batch->in .scalars   .index_put_({i, "..."}, example.in .scalars   );
		batch->out.priors    .index_put_({i, "..."}, example.out.priors    );
		batch->out.bernoullis.index_put_({i, "..."}, example.out.bernoullis);
		batch->    reachable .index_put_({i, "..."}, example.    reachable );
	}

	batch->to_gpu();
	return batch;
}

void discard_batch(Batch *batch) { delete batch; }

double train_net(Net *net, torch::optim::SGD *optim, Batch *batch) {
	optim->zero_grad();
	auto loss = detailed_loss(*net, *batch).sum();
	loss.backward();
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
	return new torch::optim::SGD((**net).parameters(), 0.000001);
}

void discard_optimizer(torch::optim::SGD *optim) { delete optim; }
