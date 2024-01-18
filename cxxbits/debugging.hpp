#include <torch/torch.h>

// #define DEBUG

struct TensorSketch {
	torch::Device dev;
	torch::ScalarType ty;
	std::vector<int> dims;
	bool grad, defined, has_nan;

	TensorSketch(torch::Tensor t);
};

std::ostream &operator<<(std::ostream &o, const TensorSketch sketch);

enum DebugVerbosity
	{ SILENT = 0
	, INFO
	, CALLS
	, VERBOSE = CALLS
	};

extern const DebugVerbosity DEFAULT_LAYER_VERBOSITY;
extern const DebugVerbosity DEFAULT_CONSTRUCTOR_VERBOSITY;
extern const DebugVerbosity DEFAULT_SERIALIZATION_VERBOSITY;
extern const DebugVerbosity DEFAULT_TRANSFER_VERBOSITY;
extern const DebugVerbosity DEFAULT_VERBOSITY;

class DebugScope {
	public:
		DebugScope(std::string nm, int vrb = DEFAULT_VERBOSITY);
		~DebugScope();

		template<typename T> std::ostream &operator<<(const T &t);

	private:
#ifdef DEBUG
		const std::string name;
		const int verbosity;
#endif
		std::ostream devnull;
};
