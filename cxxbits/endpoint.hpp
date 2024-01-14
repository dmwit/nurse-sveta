#pragma once

#include <iostream>
#include <map>
#include <string>
#include <vector>

#include <torch/torch.h>

extern "C" {
#include "endpoint.h"
}

using namespace std;

struct _dimensions;
typedef shared_ptr<_dimensions> sp_dimensions;
struct dimensions {
	sp_dimensions ref;
	dimensions(_dimensions *ref): ref(ref) {}
	dimensions(sp_dimensions ref): ref(ref) {}
};

struct _structure;
typedef shared_ptr<_structure> sp_structure;
struct structure {
	sp_structure ref;
	structure(_structure *ref): ref(ref) {}
	structure(sp_structure ref): ref(ref) {}
};

struct _endpoint;
typedef shared_ptr<_endpoint> sp_endpoint;
struct endpoint {
	sp_endpoint ref;
	endpoint(_endpoint *ref): ref(ref) {}
	endpoint(sp_endpoint ref): ref(ref) {}
};

struct _dimensions {
	dimensions_tag tag;
	vector<game_constant> constants;

	_dimensions(dimensions_tag tag, int capacity_hint = 0)
		: tag(tag)
	{ constants.reserve(capacity_hint); }

	int eval() const;
	vector<int64_t> to_vector() const;
};

struct _structure {
	structure_tag tag;
	sp_dimensions tensor_dimensions;
	map<string, sp_structure> children;

	// caller MUST choose from unit, positive, categorical, or heterogeneous
	_structure(structure_tag tag)
		: tag(tag)
	{}

	// masked
	_structure(sp_structure child)
		: tag(tag_masked)
	{ children.insert(pair<string, sp_structure>("", child)); }

	// rectangle
	_structure(sp_dimensions d, sp_structure child)
		: tag(tag_rectangle), tensor_dimensions(d)
	{ children.insert(pair<string, sp_structure>("", child)); }

	void add_child(string name, sp_structure child);
	const sp_structure child() const { return children.at(""); }
	bool is_leaf() { return tag == tag_unit || tag == tag_positive || tag == tag_categorical; }
 };

struct _endpoint {
	int size; // how large is the batch; this value is replicated at all levels
	sp_structure shape;
	map<string, sp_endpoint> heterogeneous_children;
	// Normally, rectangle structures contain leaf types like unit or positive.
	// These will be stored in rectangle_values. But they technically can
	// contain other things, so we need to be able to deal with that. In other
	// cases, the substructures will be stored in rectangle_children, with the
	// first index varying fastest.
	vector<sp_endpoint> rectangle_children;
	torch::Tensor rectangle_values;
	sp_dimensions rectangle_dimensions;
	torch::Tensor masked_values;

	_endpoint(int size, _structure *shape)
		: size(size), shape(shape)
	{}

	void add_child(string name, sp_endpoint child);
	bool has_masks() const;
	sp_structure unmasked_shape() const;
	void initialize_tensors(float *values, char *mask);
};

ostream &operator<<(ostream &os, const _dimensions &d);
ostream &operator<<(ostream &os, const _structure &d);
ostream &operator<<(ostream &os, const _endpoint &d);
