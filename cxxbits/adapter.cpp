#include <torch/torch.h>

const int64_t BOARD_WIDTH = 8;
const int64_t BOARD_HEIGHT = 16;
const int64_t LOOKAHEAD_SIZE = 6;
const int64_t CELL_SIZE = 7;
const int64_t NUM_SCALARS = 3;
const int64_t NUM_BERNOULLIS = 1;
const int64_t NUM_ROTATIONS = 4;

const int64_t NUM_CELLS = BOARD_WIDTH * BOARD_HEIGHT;

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
	torch::Tensor boards, lookaheads;

	NetInput() {}

	NetInput(int n, char *boards_data, char *lookaheads_data) {
		auto inputOptions = torch::TensorOptions().dtype(torch::kU8);
		boards     = torch::from_blob(    boards_data, {n, CELL_SIZE, BOARD_WIDTH, BOARD_HEIGHT}, [](void *v){}, inputOptions);
		lookaheads = torch::from_blob(lookaheads_data, {n, LOOKAHEAD_SIZE}                      , [](void *v){}, inputOptions);
	}

	NetInput(int n) {
		auto charOptions = torch::TensorOptions().dtype(torch::kU8);
		boards     = torch::empty({n, CELL_SIZE, BOARD_WIDTH, BOARD_HEIGHT}, charOptions);
		lookaheads = torch::empty({n, LOOKAHEAD_SIZE}                      , charOptions);
	}

	void to_gpu() {
		boards     = boards    .to(torch::kCUDA);
		lookaheads = lookaheads.to(torch::kCUDA);
		boards     = boards    .to(torch::kF64);
		lookaheads = lookaheads.to(torch::kF64);
	}

	void save(torch::serialize::OutputArchive &archive) {
		archive.write("boards"    , boards    , true);
		archive.write("lookaheads", lookaheads, true);
	}

	void load(torch::serialize::InputArchive &archive) {
		archive.read("boards"    , boards    , true);
		archive.read("lookaheads", lookaheads, true);
	}

	std::ostream &dump(std::ostream &o) {
		return o
			<<     "boards: " << boards     << std::endl
			<< "lookaheads: " << lookaheads << std::endl
			;
	}

	std::ostream &sketch(std::ostream &o) {
		return o
			<<     "boards: " << TensorSketch(boards    ) << ", "
			<< "lookaheads: " << TensorSketch(lookaheads)
			;
	}
};

std::ostream &operator<<(std::ostream &o, NetInput &in) { return in.dump(o); }

struct NetOutput {
	// priors: [n, 4, 8, 16]
	// bernoullis: [n, 1]
	// scalars: [n, 3]
	torch::Tensor priors, bernoullis, scalars;

	NetOutput() {}

	NetOutput(int n, double *priors_data, char *bernoullis_data, double *scalars_data) {
		auto doubleOptions = torch::TensorOptions().dtype(torch::kF64);
		auto charOptions   = torch::TensorOptions().dtype(torch::kU8);
		priors     = torch::from_blob(    priors_data, {n, NUM_ROTATIONS, BOARD_WIDTH, BOARD_HEIGHT}, [](void *){}, doubleOptions);
		bernoullis = torch::from_blob(bernoullis_data, {n, NUM_BERNOULLIS}                          , [](void *){}, charOptions  );
		scalars    = torch::from_blob(   scalars_data, {n, NUM_SCALARS}                             , [](void *){}, doubleOptions);
	}

	NetOutput(int n) {
		auto doubleOptions = torch::TensorOptions().dtype(torch::kF64);
		auto charOptions   = torch::TensorOptions().dtype(torch::kU8);

		priors     = torch::empty({n, NUM_ROTATIONS, BOARD_WIDTH, BOARD_HEIGHT}, doubleOptions);
		bernoullis = torch::empty({n, NUM_BERNOULLIS}                          , charOptions  );
		scalars    = torch::empty({n, NUM_SCALARS}                             , doubleOptions);
	}

	void to_gpu() {
		priors     = priors    .to(torch::kCUDA);
		bernoullis = bernoullis.to(torch::kCUDA);
		scalars    = scalars   .to(torch::kCUDA);
		bernoullis = bernoullis.to(torch::kF64);
	}

	void to_cpu() {
		priors     = priors    .to(torch::kCPU);
		bernoullis = bernoullis.to(torch::kCPU);
		scalars    = scalars   .to(torch::kCPU);
	}

	void save(torch::serialize::OutputArchive &archive) {
		archive.write("priors"    , priors    , true);
		archive.write("bernoullis", bernoullis, true);
		archive.write("scalars"   , scalars   , true);
	}

	void load(torch::serialize::InputArchive &archive) {
		archive.read("priors"    , priors    , true);
		archive.read("bernoullis", bernoullis, true);
		archive.read("scalars"   , scalars   , true);
	}

	std::ostream &dump(std::ostream &o) {
		return o
			<<     "priors: " << priors     << std::endl
			<< "bernoullis: " << bernoullis << std::endl
			<<    "scalars: " << scalars    << std::endl
			;
	}

	std::ostream &sketch(std::ostream &o) {
		return o
			<<     "priors: " << TensorSketch(priors    ) << ", "
			<< "bernoullis: " << TensorSketch(bernoullis) << ", "
			<<    "scalars: " << TensorSketch(scalars   )
			;
	}
};

std::ostream &operator<<(std::ostream &o, NetOutput &out) { return out.dump(o); }

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

	Batch(int n, double *priors_data, char *reachable_data, char *bernoullis_data, double *scalars_data, char *boards_data, char *lookaheads_data)
		: in(n, boards_data, lookaheads_data)
		, out(n, priors_data, bernoullis_data, scalars_data) {
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

class NetImpl : public torch::nn::Module {
	public:
		NetImpl()
			: torch::nn::Module("Nurse Sveta net")
			, linear(torch::nn::Linear(CELL_SIZE*NUM_CELLS + LOOKAHEAD_SIZE, NUM_ROTATIONS*NUM_CELLS + NUM_BERNOULLIS + NUM_SCALARS))
			{
				register_module("linear", linear);
				to(torch::kCUDA);
				to(torch::kF64);
			}
		NetOutput forward(const NetInput &in);

	private:
		torch::nn::Linear linear;
};
TORCH_MODULE(Net);

NetOutput NetImpl::forward(const NetInput &in) {
	const int n = in.boards.size(0);
	auto linear_in = torch::cat(
		{ in.boards.reshape({n, CELL_SIZE*NUM_CELLS})
		, in.lookaheads
		}, 1);
	auto linear_out = linear->forward(linear_in);

	NetOutput out;
	int64_t start = 0, len;
	len = NUM_ROTATIONS*NUM_CELLS; out.priors     = linear_out.index({torch::indexing::Slice(), torch::indexing::Slice(start, start + len)}); start += len;
	len = NUM_BERNOULLIS         ; out.bernoullis = linear_out.index({torch::indexing::Slice(), torch::indexing::Slice(start, start + len)}); start += len;
	len = NUM_SCALARS            ; out.scalars    = linear_out.index({torch::indexing::Slice(), torch::indexing::Slice(start, start + len)}); start += len;
	out.priors = out.priors.reshape({n, NUM_ROTATIONS, BOARD_WIDTH, BOARD_HEIGHT}).exp();
	out.bernoullis = out.bernoullis.sigmoid();

	return out;
}

torch::Tensor compute_loss(const NetOutput &correct_out, const torch::Tensor &reachable, const NetOutput &net_out) {
	const int n = correct_out.priors.size(0);
	auto opts = torch::TensorOptions().dtype(torch::kF64).device(torch::kCUDA);
	auto loss = torch::zeros({}, opts);
	// Kullback-Leibler divergence for priors
	auto scaled_priors = net_out.priors / (reachable*net_out.priors).sum({1,2,3}, true);
	// correct_out.priors can have zeros (e.g. everywhere reachable is zero),
	// which leads to -inf's after the log, which leads to nan's in the
	// gradients. I also tried clamping after the log, but the nan's show up
	// even after multiplying by reachable to screen off the effect of the
	// incoming zeros.
	loss += (correct_out.priors * (correct_out.priors.clamp_min(1e-20) / scaled_priors).log() * reachable).sum();
	// cross-entropy loss for bernoullis
	auto bs = net_out.bernoullis.clamp(1e-10, 1-1e-10);
	loss -= (correct_out.bernoullis * bs.log() + (1 - correct_out.bernoullis) * (1 - bs).log()).sum();
	// squared-error loss for scalars
	// this is about 1e8 times as big as the bernoullis terms when training
	// starts; other than that there's no real reason to believe 1e8 is the
	// right factor here
	loss += (net_out.scalars - correct_out.scalars).square().sum() / 1e8;
	loss /= n;
	return loss;
}

torch::Tensor compute_loss(Net &net, const Batch &batch) {
	return compute_loss(batch.out, batch.reachable, net->forward(batch.in));
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
	void discard_net(Net *net);
	void evaluate_net(Net *net, int n, double *priors, double *bernoullis, double *scalars, char *boards, char *lookaheads);
	void save_example(char *path, double *priors, char *reachable, char *bernoullis, double *scalars, char *board, char *lookahead);
	Batch *load_batch(char **path, int n);
	void discard_batch(Batch *batch);
	double train_net(Net *net, torch::optim::SGD *optim, Batch *batch);
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

// priors: [n, 4, 8, 16]
// bernoullis: [n, 1]
// scalars: [n, 3]
// boards: [n, 7, 8, 16]
// lookaheads: [n, 6]
void evaluate_net(Net *net, int n, double *priors, double *bernoullis, double *scalars, char *boards, char *lookaheads) {
	NetInput in(n, boards, lookaheads); in.to_gpu();
	NetOutput out = (**net).forward(in); out.to_cpu();

	tensorcpy(priors,     out.priors    );
	tensorcpy(bernoullis, out.bernoullis);
	tensorcpy(scalars,    out.scalars   );
}

// priors: [4, 8, 16]
// reachable: [4, 8, 16]
// bernoullis: [1]
// scalars: [3]
// board: [7, 8, 16]
// lookahead: [6]
void save_example(char *path, double *priors, char *reachable, char *bernoullis, double *scalars, char *board, char *lookahead) {
	Batch batch(1, priors, reachable, bernoullis, scalars, board, lookahead);
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
		batch->out.priors    .index_put_({i, "..."}, example.out.priors    );
		batch->out.bernoullis.index_put_({i, "..."}, example.out.bernoullis);
		batch->out.scalars   .index_put_({i, "..."}, example.out.scalars   );
		batch->    reachable .index_put_({i, "..."}, example.    reachable );
	}

	batch->to_gpu();
	return batch;
}

void discard_batch(Batch *batch) { delete batch; }

double train_net(Net *net, torch::optim::SGD *optim, Batch *batch) {
	optim->zero_grad();
	auto loss = compute_loss(*net, *batch).to(torch::kCPU);
	loss.backward();
	optim->step();
	return *loss.data_ptr<double>();
}

torch::optim::SGD *connect_optimizer(Net *net) {
	// TODO: allow setting SGD parameters like momentum, learning rate, etc.
	return new torch::optim::SGD((**net).parameters(), 0.00001);
}

void discard_optimizer(torch::optim::SGD *optim) { delete optim; }
