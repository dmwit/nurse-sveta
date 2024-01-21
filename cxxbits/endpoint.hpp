#pragma once

#include <iostream>
#include <map>
#include <string>
#include <vector>

#include <torch/torch.h>

extern "C" {
#include "endpoint.h"
}

int eval_game_constants(std::vector<game_constant> cs);
std::vector<int64_t> tensor_dimensions(int64_t batch_size, const std::vector<game_constant> &dims);

struct _structure;
typedef std::shared_ptr<_structure> sp_structure;
struct structure {
	sp_structure ref;
	structure(_structure *ref): ref(ref) {}
	structure(sp_structure ref): ref(ref) {}
};

struct _endpoint;
typedef std::shared_ptr<_endpoint> sp_endpoint;
struct endpoint {
	sp_endpoint ref;
	endpoint(_endpoint *ref): ref(ref) {}
	endpoint(sp_endpoint ref): ref(ref) {}
};

struct _structure {
	structure_tag tag;
	leaf_type ty;
	std::vector<game_constant> dims; // will have size 1 for tag_vector
	sp_structure vec;
	std::map<std::string, sp_structure> dict;

	void add_child(std::string name, sp_structure child);
 };

struct _endpoint {
	int size; // how large is the batch; this value is replicated at all levels, and may be -1 to indicate an indeterminate size
	structure_tag tag;
	std::vector<game_constant> dims; // will have size 1 for tag_vector
	torch::Tensor values, mask;
	std::vector<sp_endpoint> vec;
	std::map<std::string, sp_endpoint> dict;

	void add_child(std::string name, sp_endpoint child);
	bool assert_size(int other_size);
};

std::ostream &dump_game_constant(std::ostream &os, const game_constant c);
std::ostream &dump_game_constants(std::ostream &os, const std::vector<game_constant> &gcs);
std::ostream &dump_leaf_type(std::ostream &os, const leaf_type ty);
std::ostream &operator<<(std::ostream &os, const _structure &d);
std::ostream &operator<<(std::ostream &os, const _endpoint &d);
