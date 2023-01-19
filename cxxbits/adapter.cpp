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
	void discard(Net *n);
	void evaluate(Net *n, double *priors, double *valuation, double *scalars, const double *boards, const double *lookaheads);
}

Net *sample() { return new Net(); }
void discard(Net *n) { delete n; }
void evaluate(Net *n, double *priors, double *valuation, double *scalars, const double *boards, const double *lookaheads) {
	std::cout << "evaluating" << std::endl;
}
