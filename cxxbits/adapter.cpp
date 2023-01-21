#include <iostream>
#include <torch/torch.h>

class Net {
	public:
		Net() { std::cout << "birthing" << std::endl; }
		~Net() { std::cout << "dying" << std::endl; }
	private:
};

extern "C" {
	Net *sample();
	void discard(Net *net);
	void evaluate(Net *net, int n, double *priors, double *valuation, double *scalars, const double *boards, const double *lookaheads);
}

Net *sample() { return new Net(); }
void discard(Net *net) { delete net; }

// priors: [n, 4, 8, 16]
// valuation: [n]
// scalars: [n, 3]
// boards: [n, 7, 8, 16]
// lookaheads: [n, 6]
void evaluate(Net *net, int n, double *priors, double *valuation, double *scalars, const double *boards, const double *lookaheads) {
	std::cout << "evaluating " << n << std::endl;
}
