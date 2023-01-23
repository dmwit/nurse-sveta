#include <iostream>
#include <torch/torch.h>

const int64_t BOARD_WIDTH = 8;
const int64_t BOARD_HEIGHT = 16;
const int64_t LOOKAHEAD_SIZE = 6;
const int64_t CELL_SIZE = 7;
const int64_t NUM_SCALARS = 3;
const int64_t NUM_VALUATIONS = 1;
const int64_t NUM_ROTATIONS = 4;

const int64_t NUM_CELLS = BOARD_WIDTH * BOARD_HEIGHT;

class NetImpl : torch::nn::Module {
	public:
		NetImpl(bool training)
			: torch::nn::Module("Nurse Sveta net")
			, linear(torch::nn::Linear(CELL_SIZE*NUM_CELLS + LOOKAHEAD_SIZE, NUM_ROTATIONS*NUM_CELLS + NUM_VALUATIONS + NUM_SCALARS))
			, training(training)
			{
				register_module("linear", linear);
				to(torch::kCUDA);
			}
		torch::Tensor forward(const torch::Tensor& boards, const torch::Tensor& lookaheads);

	private:
		const bool training;
		torch::nn::Linear linear;
};
TORCH_MODULE(Net);

torch::Tensor NetImpl::forward(const torch::Tensor &boards, const torch::Tensor& lookaheads) {
	auto guard = training ? NULL : new torch::NoGradGuard();
	auto outputOptions = torch::TensorOptions()
		.dtype(torch::kF64)
		.device(torch::kCUDA);
	// TODO
	delete guard;
	return torch::ones({boards.size(0), linear->weight.size(0)}, outputOptions);
}

void tensorcpy(double *out, torch::Tensor &in, int64_t *start, int64_t len) {
	if(in.dtype() != torch::kF64) throw 0;
	// TODO: is it safe to inline the definition of contig_in, or will that
	// lead to the Tensor being destructed before memcpy finishes?
	auto contig_in = in.slice(1, *start, *start+len).contiguous();
	std::cout << contig_in << std::endl;
	std::cout << "tensorcpy size " << in.size(0) << std::endl;
	std::memcpy(out, contig_in.data_ptr<double>(), in.size(0)*len*sizeof(double));
	std::cout << "tensorcpy out: ";
	for(int i=0; i < in.size(0)*len; i++) {
		std::cout << out[i] << " ";
	}
	std::cout << std::endl;
	*start += len;
}

extern "C" {
	Net *sample(bool training);
	void discard(Net *net);
	void evaluate(Net *net, int n, double *priors, double *valuation, double *scalars, double *boards, double *lookaheads);
}

Net *sample(bool training) { return new Net(training); }
void discard(Net *net) { delete net; }

// priors: [n, 4, 8, 16]
// valuation: [n]
// scalars: [n, 3]
// boards: [n, 7, 8, 16]
// lookaheads: [n, 6]
void evaluate(Net *net, int n, double *priors, double *valuation, double *scalars, double *boards, double *lookaheads) {
	auto inputOptions = torch::TensorOptions().dtype(torch::kF64);
	auto boards_tensor = torch::from_blob(boards, {n, CELL_SIZE, BOARD_WIDTH, BOARD_HEIGHT}, [](void *v){}, inputOptions).to(torch::kCUDA);
	auto lookaheads_tensor = torch::from_blob(lookaheads, {n, LOOKAHEAD_SIZE}, [](void *v){}, inputOptions).to(torch::kCUDA);
	auto output_tensor = (**net).forward(boards_tensor, lookaheads_tensor).to(torch::kCPU);

	// std::cout << output_tensor.dim() << " " << output_tensor.size(0) << " " << output_tensor.size(1) << " " << NUM_ROTATIONS*NUM_CELLS << " " << NUM_ROTATIONS*NUM_CELLS+NUM_VALUATIONS << " " << NUM_ROTATIONS*NUM_CELLS+NUM_VALUATIONS+NUM_SCALARS << std::endl;
	output_tensor.index_put_({torch::indexing::Slice(), torch::indexing::Slice(NUM_ROTATIONS*NUM_CELLS, NUM_ROTATIONS*NUM_CELLS+NUM_VALUATIONS)}, 2);
	output_tensor.index_put_({torch::indexing::Slice(), torch::indexing::Slice(NUM_ROTATIONS*NUM_CELLS+NUM_VALUATIONS, NUM_ROTATIONS*NUM_CELLS+NUM_VALUATIONS+NUM_SCALARS)}, 3);
	// std::cout << output_tensor[0] << std::endl;

	int64_t start = 0;
	// std::cout << output_tensor << std::endl;
	std::cout << "evaluate size " << n << std::endl;
	tensorcpy(priors,    output_tensor, &start, NUM_ROTATIONS*NUM_CELLS);
	tensorcpy(valuation, output_tensor, &start, NUM_VALUATIONS);
	tensorcpy(scalars,   output_tensor, &start, NUM_SCALARS);

	std::cout << "cxx-side valuation: ";
	for(int i = 0; i < n; i++) std::cout << valuation[i] << " ";
	std::cout << std::endl;
}

int main() {
	Net *net = sample(false);
	double boards[CELL_SIZE*NUM_CELLS*9], lookaheads[LOOKAHEAD_SIZE*9];
	double priors[NUM_ROTATIONS*NUM_CELLS*9], valuation[NUM_VALUATIONS*9], scalars[NUM_SCALARS*9];
	evaluate(net, 9, priors, valuation, scalars, boards, lookaheads);
	discard(net);
}
