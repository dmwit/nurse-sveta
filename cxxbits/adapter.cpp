#include <iostream>
#include <torch/torch.h>

extern "C" { void ffi_demo(); }

void ffi_demo() {
	auto opts = torch::device(torch::kCUDA);
	auto tensor = torch::rand({2, 3}, opts);
	std::cout << tensor << std::endl;
}
