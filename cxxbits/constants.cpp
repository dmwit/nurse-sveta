#include "constants.hpp"

const torch::TensorOptions CPU_BYTE  = torch::TensorOptions().dtype(torch::kU8);
const torch::TensorOptions CPU_FLOAT = torch::TensorOptions().dtype(torch::kF32);
const torch::TensorOptions GPU_BYTE  = CPU_BYTE.device(torch::kCUDA);
const torch::TensorOptions GPU_FLOAT = CPU_FLOAT.device(torch::kCUDA);

const int64_t BOARD_WIDTH = 8;
const int64_t BOARD_HEIGHT = 16;
const int64_t ROTATIONS = 4;
const int64_t ORIENTATIONS = 2; // horizontal, vertical
const int64_t COLORS = 3; // blue, red, yellow
const int64_t SHAPES = 4; // east, west, disconnected, virus
