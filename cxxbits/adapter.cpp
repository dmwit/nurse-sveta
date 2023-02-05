#include <torch/torch.h>

const int64_t BOARD_WIDTH = 8;
const int64_t BOARD_HEIGHT = 16;
const int64_t LOOKAHEAD_SIZE = 6;
const int64_t CELL_SIZE = 7;
const int64_t NUM_SCALARS = 3;
const int64_t NUM_BERNOULLIS = 1;
const int64_t NUM_ROTATIONS = 4;

const int64_t NUM_CELLS = BOARD_WIDTH * BOARD_HEIGHT;

class NetImpl : torch::nn::Module {
	public:
		NetImpl(bool training)
			: torch::nn::Module("Nurse Sveta net")
			, linear(torch::nn::Linear(CELL_SIZE*NUM_CELLS + LOOKAHEAD_SIZE, NUM_ROTATIONS*NUM_CELLS + NUM_BERNOULLIS + NUM_SCALARS))
			, training(training)
			{
				register_module("linear", linear);
				to(torch::kCUDA);
				to(torch::kF64);
			}
		torch::Tensor forward(const torch::Tensor& boards, const torch::Tensor& lookaheads);

	private:
		const bool training;
		torch::nn::Linear linear;
};
TORCH_MODULE(Net);

torch::Tensor NetImpl::forward(const torch::Tensor &boards, const torch::Tensor& lookaheads) {
	// TODO: does this... actually work??
	auto guard = training ? NULL : new torch::NoGradGuard();
	auto outputOptions = torch::TensorOptions()
		.dtype(torch::kF64)
		.device(torch::kCUDA);

	auto linear_input = torch::cat(
		{ boards.reshape({boards.size(0), CELL_SIZE*NUM_CELLS})
		, lookaheads
		}, 1);

	auto linear_output = linear->forward(linear_input);
	delete guard;
	return linear_output;
}

struct Batch {
	torch::Tensor priors, reachable, bernoullis, scalars, board, lookahead;

	Batch(int n) {
		auto doubleOptions = torch::TensorOptions().dtype(torch::kF64);
		auto charOptions   = torch::TensorOptions().dtype(torch::kU8);

		priors     = torch::empty({n, NUM_ROTATIONS, BOARD_WIDTH, BOARD_HEIGHT}, doubleOptions);
		reachable  = torch::empty({n, NUM_ROTATIONS, BOARD_WIDTH, BOARD_HEIGHT}, charOptions  );
		bernoullis = torch::empty({n, NUM_BERNOULLIS}                          , charOptions  );
		scalars    = torch::empty({n, NUM_SCALARS}                             , doubleOptions);
		board      = torch::empty({n, CELL_SIZE, BOARD_WIDTH, BOARD_HEIGHT}    , charOptions  );
		lookahead  = torch::empty({n, LOOKAHEAD_SIZE}                          , charOptions  );
	}

	void to_gpu() {
		priors     = priors    .to(torch::kCUDA);
		reachable  = reachable .to(torch::kCUDA);
		bernoullis = bernoullis.to(torch::kCUDA);
		scalars    = scalars   .to(torch::kCUDA);
		board      = board     .to(torch::kCUDA);
		lookahead  = lookahead .to(torch::kCUDA);

		reachable  = reachable .to(torch::kF64);
		bernoullis = bernoullis.to(torch::kF64);
		board      = board     .to(torch::kF64);
		lookahead  = lookahead .to(torch::kF64);
	}
};

void tensorcpy(double *out, torch::Tensor &in, int64_t *start, int64_t len) {
	if(in.dtype() != torch::kF64) throw 0;
	// TODO: is it safe to inline the definition of contig_in, or will that
	// lead to the Tensor being destructed before memcpy finishes?
	auto contig_in = in.slice(1, *start, *start+len).contiguous();
	std::memcpy(out, contig_in.data_ptr<double>(), in.size(0)*len*sizeof(double));
	*start += len;
}

extern "C" {
	Net *sample_net(bool training);
	void discard_net(Net *net);
	void evaluate_net(Net *net, int n, double *priors, double *bernoullis, double *scalars, double *boards, double *lookaheads);
	void save_example(char *path, double *priors, char *reachable, char *bernoullis, double *scalars, char *board, char *lookahead);
	Batch *load_batch(char **path, int n);
	void discard_batch(Batch *batch);
}

Net *sample_net(bool training) { return new Net(training); }
void discard_net(Net *net) { delete net; }

// priors: [n, 4, 8, 16]
// bernoullis: [n]
// scalars: [n, 3]
// boards: [n, 7, 8, 16]
// lookaheads: [n, 6]
void evaluate_net(Net *net, int n, double *priors, double *bernoullis, double *scalars, double *boards, double *lookaheads) {
	auto inputOptions = torch::TensorOptions().dtype(torch::kF64);
	auto boards_tensor = torch::from_blob(boards, {n, CELL_SIZE, BOARD_WIDTH, BOARD_HEIGHT}, [](void *v){}, inputOptions).to(torch::kCUDA);
	auto lookaheads_tensor = torch::from_blob(lookaheads, {n, LOOKAHEAD_SIZE}, [](void *v){}, inputOptions).to(torch::kCUDA);
	auto output_tensor = (**net).forward(boards_tensor, lookaheads_tensor).to(torch::kCPU);

	int64_t start = 0;
	tensorcpy(priors,     output_tensor, &start, NUM_ROTATIONS*NUM_CELLS);
	tensorcpy(bernoullis, output_tensor, &start, NUM_BERNOULLIS);
	tensorcpy(scalars,    output_tensor, &start, NUM_SCALARS);
}

// priors: [4, 8, 16]
// reachable: [4, 8, 16]
// bernoullis: [1]
// scalars: [3]
// board: [7, 8, 16]
// lookahead: [6]
void save_example(char *path, double *priors, char *reachable, char *bernoullis, double *scalars, char *board, char *lookahead) {
	auto doubleOptions = torch::TensorOptions().dtype(torch::kF64);
	auto charOptions   = torch::TensorOptions().dtype(torch::kU8);

	auto priors_tensor = torch::from_blob(priors, {NUM_ROTATIONS, BOARD_WIDTH, BOARD_HEIGHT}, [](void *){}, doubleOptions);
	auto reachable_tensor = torch::from_blob(reachable, {NUM_ROTATIONS, BOARD_WIDTH, BOARD_HEIGHT}, [](void *){}, charOptions);
	auto bernoullis_tensor = torch::from_blob(bernoullis, {NUM_BERNOULLIS}, [](void *){}, charOptions);
	auto scalars_tensor = torch::from_blob(scalars, {NUM_SCALARS}, [](void *){}, doubleOptions);
	auto board_tensor = torch::from_blob(board, {CELL_SIZE, BOARD_WIDTH, BOARD_HEIGHT}, [](void *){}, charOptions);
	auto lookahead_tensor = torch::from_blob(lookahead, {LOOKAHEAD_SIZE}, [](void *){}, charOptions);

	torch::serialize::OutputArchive archive;
	archive.write("priors"    ,     priors_tensor, true);
	archive.write("reachable" ,  reachable_tensor, true);
	archive.write("bernoullis", bernoullis_tensor, true);
	archive.write("scalars"   ,    scalars_tensor, true);
	archive.write("board"     ,      board_tensor, true);
	archive.write("lookahead" ,  lookahead_tensor, true);

	archive.save_to(path);
}

Batch *load_batch(char **path, int n) {
	Batch *batch = new Batch(n);
	torch::serialize::InputArchive archive;
	torch::Tensor priors, reachable, bernoullis, scalars, board, lookahead;

	for(int i = 0; i < n; i++) {
		archive.load_from(path[i], c10::optional<torch::Device>(torch::kCPU));
		archive.read("priors"    , priors    , true); batch->priors    .index_put_({i , "..."}, priors    );
		archive.read("reachable" , reachable , true); batch->reachable .index_put_({i , "..."}, reachable );
		archive.read("bernoullis", bernoullis, true); batch->bernoullis.index_put_({i , "..."}, bernoullis);
		archive.read("scalars"   , scalars   , true); batch->scalars   .index_put_({i , "..."}, scalars   );
		archive.read("board"     , board     , true); batch->board     .index_put_({i , "..."}, board     );
		archive.read("lookahead" , lookahead , true); batch->lookahead .index_put_({i , "..."}, lookahead );
	}

	batch->to_gpu();
	return batch;
}

void discard_batch(Batch *batch) { delete batch; }
