#include "debugging.hpp"

TensorSketch::TensorSketch(torch::Tensor t)
	: dev(t.defined()?t.device():c10::Device("cpu"))
{
	defined = t.defined();
	if(defined) {
		ty = t.scalar_type();
		grad = t.requires_grad();
		has_nan = at::isnan(t).any().item<bool>();
		for(int i = 0; i < t.dim(); i++) dims.push_back(t.size(i));
	}
}

std::ostream &operator<<(std::ostream &o, const TensorSketch sketch) {
	if(sketch.defined) {
		o << sketch.ty << "[";
		if(sketch.dims.size() > 0) {
			o << sketch.dims[0];
			for(int i = 1; i < sketch.dims.size(); i++)
				o << ", " << sketch.dims[i];
		}
		return o << "]@" << sketch.dev << (sketch.grad?"+":"-") << (sketch.has_nan?"!":"");
	} else {
		return o << "<undefined>";
	}
}

const DebugVerbosity DEFAULT_LAYER_VERBOSITY = INFO;
const DebugVerbosity DEFAULT_CONSTRUCTOR_VERBOSITY = INFO;
const DebugVerbosity DEFAULT_SERIALIZATION_VERBOSITY = INFO;
const DebugVerbosity DEFAULT_TRANSFER_VERBOSITY = INFO;
const DebugVerbosity DEFAULT_VERBOSITY = VERBOSE;

DebugScope::DebugScope(std::string nm, int vrb)
	: devnull(0) // https://stackoverflow.com/q/7818371/791604
#ifdef DEBUG
	, name(nm)
	, verbosity(vrb)
{ if(verbosity >= CALLS) std::cerr << "entering " << name << std::endl; }
#else
{}
#endif

DebugScope::~DebugScope() {
#ifdef DEBUG
	if(verbosity >= CALLS) std::cerr << "exiting  " << name << std::endl;
#endif
}

template<typename T> std::ostream &DebugScope::operator<<(const T &t) {
#ifdef DEBUG
	if(verbosity >= INFO)
		return std::cerr << "\t" << name << ": " << t;
#endif
	return devnull;
}
