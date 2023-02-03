#include <iostream>
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

void tensorcpy(double *out, torch::Tensor &in, int64_t *start, int64_t len) {
	if(in.dtype() != torch::kF64) throw 0;
	// TODO: is it safe to inline the definition of contig_in, or will that
	// lead to the Tensor being destructed before memcpy finishes?
	auto contig_in = in.slice(1, *start, *start+len).contiguous();
	std::memcpy(out, contig_in.data_ptr<double>(), in.size(0)*len*sizeof(double));
	*start += len;
}

extern "C" {
	Net *sample(bool training);
	void discard(Net *net);
	void evaluate(Net *net, int n, double *priors, double *bernoullis, double *scalars, double *boards, double *lookaheads);
}

Net *sample(bool training) { return new Net(training); }
void discard(Net *net) { delete net; }

// priors: [n, 4, 8, 16]
// bernoullis: [n]
// scalars: [n, 3]
// boards: [n, 7, 8, 16]
// lookaheads: [n, 6]
void evaluate(Net *net, int n, double *priors, double *bernoullis, double *scalars, double *boards, double *lookaheads) {
	auto inputOptions = torch::TensorOptions().dtype(torch::kF64);
	auto boards_tensor = torch::from_blob(boards, {n, CELL_SIZE, BOARD_WIDTH, BOARD_HEIGHT}, [](void *v){}, inputOptions).to(torch::kCUDA);
	auto lookaheads_tensor = torch::from_blob(lookaheads, {n, LOOKAHEAD_SIZE}, [](void *v){}, inputOptions).to(torch::kCUDA);
	auto output_tensor = (**net).forward(boards_tensor, lookaheads_tensor).to(torch::kCPU);

	int64_t start = 0;
	tensorcpy(priors,     output_tensor, &start, NUM_ROTATIONS*NUM_CELLS);
	tensorcpy(bernoullis, output_tensor, &start, NUM_BERNOULLIS);
	tensorcpy(scalars,    output_tensor, &start, NUM_SCALARS);
}
