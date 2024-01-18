#include "endpoint.hpp"
#include "constants.hpp"

using namespace std;

int eval_game_constant(game_constant c) {
	switch(c) {
		case c_colors: return COLORS;
		case c_shapes: return SHAPES;
		case c_width: return BOARD_WIDTH;
		case c_height: return BOARD_HEIGHT;
		case c_orientations: return ORIENTATIONS;
		default: throw 0;
	}
}

void dump_game_constant(game_constant c) { flush(cout << c); }
ostream &operator<<(ostream &os, const game_constant &c) {
	return os << eval_game_constant(c);
}

int _dimensions::eval() const {
	int n;
	switch(tag) {
		case tag_product:
			n = 1;
			for(auto c: constants) n *= eval_game_constant(c);
			break;
		case tag_sum:
			n = 0;
			for(auto c: constants) n += eval_game_constant(c);
			break;
		default:
			cerr << "Invalid dimensions tag " << tag << endl;
			throw 0;
	}
	return n;
}

vector<int64_t> _dimensions::to_vector() const {
	vector<int64_t> d;
	switch(tag) {
		case tag_product:
			d.reserve(constants.size());
			for(auto c: constants) d.push_back(eval_game_constant(c));
			break;
		case tag_sum:
			d.push_back(0);
			for(auto c: constants) d[0] += eval_game_constant(c);
			break;
		default:
			cerr << "Invalid dimensions tag " << tag << endl;
			throw 0;
	}
	return d;
}

dimensions *new_dimensions_product(int capacity_hint) { return new dimensions(new _dimensions(tag_product, capacity_hint)); }
dimensions *new_dimensions_sum(int capacity_hint) { return new dimensions(new _dimensions(tag_sum, capacity_hint)); }
void dimensions_add_constant(dimensions *d, game_constant c) { d->ref->constants.push_back(c); }
dimensions_tag dimensions_get_tag(dimensions *d) { return d->ref->tag; }
void dimensions_read(int *ret_length, game_constant **ret_constants, dimensions *d) {
	auto constants = d->ref->constants;
	*ret_length = constants.size();
	*ret_constants = new game_constant[*ret_length];
	copy(constants.begin(), constants.end(), *ret_constants);
}

void free_dimensions_constants(game_constant *constants) { delete constants; }
void free_dimensions(dimensions *d) { delete d; }

void dump_dimensions(dimensions *d) { flush(cout << *(d->ref)); }
ostream &operator<<(ostream &os, const _dimensions &d) {
	char identity = '!';
	char op = '!';
	switch(d.tag) {
		case tag_product: identity = '1'; op = 'x'; break;
		case tag_sum: identity = '0'; op = '+'; break;
		default:
			cerr << "Invalid dimensions tag " << d.tag << endl;
			throw 0;
	}

	if(d.constants.size() == 0) os << identity;
	else os << d.constants[0];
	for(int i = 1; i < d.constants.size(); ++i)
		os << op << d.constants[i];
	return os;
}

structure *new_structure_unit() { return new structure(new _structure(tag_unit)); }
structure *new_structure_positive() { return new structure(new _structure(tag_positive)); }
structure *new_structure_categorical() { return new structure(new _structure(tag_categorical)); }
structure *new_structure_heterogeneous() { return new structure(new _structure(tag_heterogeneous)); }
structure *new_structure_masked(structure *child) { return new structure(new _structure(child->ref)); }
structure *new_structure_rectangle(dimensions *d, structure *child) { return new structure(new _structure(d->ref, child->ref)); }

void structure_add_child(structure *parent, char *name, structure *child) { parent->ref->add_child(name, child->ref); }
void _structure::add_child(string name, sp_structure child) {
	auto die = [&](string err) {
		cerr << err << endl;
		cerr << "Details on attempted addition:" << endl;
		cerr << "\tParent: " << *this << endl;
		cerr << "\tName: " << name << endl;
		cerr << "\tChild: " << *child << endl;
		throw 0;
	};

	if(tag != tag_heterogeneous) die("Adding children is only allowed for heterogeneous structures.");
	if(!children.emplace(name, child).second)
		die("Attempted to add a child to a heterogeneous structure that already contained that name.");
}

void free_structure(structure *s) { delete s; }

ostream &dump_leaf_type(ostream &os, const structure_tag tag) {
	switch(tag) {
		case tag_unit: return os << "sigmoid";
		case tag_positive: return os << "exp";
		case tag_categorical: return os << "softmax";
		default:
			cerr << "Invalid leaf type " << tag << endl;
			throw 0;
	}
}

void dump_structure(structure *s) { flush(cout << *(s->ref)); }
ostream &operator<<(ostream &os, const _structure &s) {
	switch(s.tag) {
		case tag_unit:
		case tag_positive:
		case tag_categorical: return dump_leaf_type(os, s.tag);
		case tag_masked: return os << '!' << *s.child();
		case tag_rectangle: return os << *s.tensor_dimensions << ' ' << *s.child();
		case tag_heterogeneous:
			os << '{';
			{ auto it(s.children.begin());
				if(it != s.children.end()) while(true) {
					// TODO: escape it->first in some sensible way
					os << it->first << ": " << *it->second;
					++it;
					if(it == s.children.end()) break;
					os << ", ";
				}
			}
			return os << '}';
		default:
			cerr << "Invalid structure tag " << s.tag << endl;
			throw 0;
	}
}

endpoint *new_endpoint_leaf(structure_tag tag, int size, float *values, char *mask) {
	_endpoint *e = new _endpoint(size, new _structure(tag));
	e->initialize_tensors(values, mask);
	return new endpoint(e);
}

endpoint *new_endpoint_unit(int size, float *values, char *mask) { return new_endpoint_leaf(tag_unit, size, values, mask); }
endpoint *new_endpoint_positive(int size, float *values, char *mask) { return new_endpoint_leaf(tag_positive, size, values, mask); }
endpoint *new_endpoint_categorical(int size, float *values, char *mask) { return new_endpoint_leaf(tag_categorical, size, values, mask); }

endpoint *new_endpoint_masked(endpoint *child) {
	if(!child->ref->has_masks()) {
		cerr << "Attempted to declare an endpoint as masked, but some children do not have masks." << endl;
		cerr << "Child endpoint: " << *(child->ref) << endl;
		throw 0;
	}

	_endpoint *e = new _endpoint(*(child->ref));
	e->shape = sp_structure(new _structure(e->shape));
	return new endpoint(e);
}

endpoint *new_endpoint_rectangle(int size, structure_tag child_type, dimensions *d, float *values, char *mask) {
	_endpoint *e = new _endpoint(size, new _structure(d->ref, sp_structure(new _structure(child_type))));
	e->rectangle_dimensions = d->ref;
	e->initialize_tensors(values, mask);
	return new endpoint(e);
}

endpoint *new_endpoint_heterogeneous(int size) { return new endpoint(new _endpoint(size, new _structure(tag_heterogeneous))); }
void endpoint_add_child(endpoint *parent, char *name, endpoint *child) { parent->ref->add_child(name, child->ref); }
void _endpoint::add_child(string name, sp_endpoint child) {
	auto die = [&](string err) {
		cerr << err << endl;
		cerr << "Details on attempted addition:" << endl;
		cerr << "\tParent: " << *this << endl;
		cerr << "\tName: " << name << endl;
		cerr << "\tChild: " << *child << endl;
		throw 0;
	};

	if(shape->tag != tag_heterogeneous) die("Adding children is only allowed for heterogeneous endpoints.");
	if(size != child->size) die("Parent endpoint and child endpoint have differing batch sizes (" + to_string(size) + " in parent, " + to_string(child->size) + " in child).");
	if(!heterogeneous_children.emplace(name, child).second)
		die("Attempted to add a child to a heterogeneous endpoint that already contained that name.");
}

structure_tag endpoint_get_tag(endpoint *e) { return e->ref->shape->tag; }

void endpoint_get_child_names(int *ret_size, char ***ret_names, endpoint *_e) {
	auto e = _e->ref;
	int i = 0;
	*ret_names = new char *[*ret_size = e->heterogeneous_children.size()];
	for(auto pair: e->heterogeneous_children) {
		(*ret_names)[i] = new char[pair.first.size()+1];
		copy(pair.first.begin(), pair.first.end(), (*ret_names)[i]);
		++i;
	}
}

endpoint *endpoint_get_named_child(endpoint *_parent, char *name) {
	auto parent = _parent->ref;

	// This *should* be redundant. That is, if the tag is not heterogeneous, it
	// ought to be the case that the heterogeneous_children field is empty, and
	// so the next try-catch will fall into the catch block. But let's just be
	// super paranoid.
	if(parent->shape->tag != tag_heterogeneous) {
		cerr << "Retrieving children is only allowed for heterogeneous endpoints." << endl;
		cerr << "Details on the attempted retrieval:" << endl;
		cerr << "\tParent: " << *parent << endl;
		cerr << "\tName: " << name << endl;
		throw 0;
	}

	try { return new endpoint(parent->heterogeneous_children.at(name)); }
	catch(const out_of_range &err) {
		cerr << err.what() << endl;
		cerr << "Current shape is " << *(parent->shape) << endl;
		throw err;
	}
}

endpoint *endpoint_get_masked_child(endpoint *_parent) {
	auto parent = _parent->ref;
	if(parent->shape->tag != tag_masked) {
		cerr << "Retrieving the masked child is only allowed for masked endpoints." << endl;
		cerr << "Endpoint was " << *parent << endl;
		throw 0;
	}
	auto child = new _endpoint(*parent);
	child->shape = child->shape->child();
	return new endpoint(child);
}

dimensions *endpoint_get_dimensions(endpoint *_e) {
	auto e = _e->ref;
	if(e->shape->tag != tag_rectangle) {
		cerr << "Retrieving dimensions is only allowed for rectangle endpoints." << endl;
		cerr << "Endpoint was " << *e << endl;
		throw 0;
	}

	return new dimensions(e->rectangle_dimensions);
}

void endpoint_read(structure_tag *ret_tag, int *ret_size, float **ret_values, endpoint *_e) {
	auto e = _e->ref;
	int len = e->size;

	*ret_tag = e->shape->tag;
	*ret_size = e->size;
	*ret_values = NULL;

	switch(e->shape->tag) {
		case tag_unit:
		case tag_positive:
		case tag_categorical:
			break;
		case tag_rectangle:
			switch(e->shape->child()->tag) {
				case tag_unit:
				case tag_positive:
				case tag_categorical:
					*ret_tag = e->shape->child()->tag;
					len *= e->rectangle_dimensions->eval();
					break;
				default:
					cerr << "Reading endpoints is only allowed for unit, positive, categorical, or the rectangular version of these." << endl;
					cerr << "Attempted to read from " << *e << endl;
					throw 0;
			}
			break;
		default:
			cerr << "Reading endpoints is only allowed for unit, positive, categorical, or the rectangular version of these." << endl;
			cerr << "Attempted to read from " << *e << endl;
			throw 0;
	}

	*ret_values = new float[len];
	// TODO: is it safe to inline the definition of contiguous, or will that
	// lead to the Tensor being destructed before memcpy finishes?
	auto contiguous = e->rectangle_values.to(torch::kCPU).contiguous();
	memcpy(*ret_values, contiguous.data_ptr<float>(), len*sizeof(**ret_values));
}

void _endpoint::initialize_tensors(float *values, char *mask) {
	vector<int64_t> dims;
	if(rectangle_dimensions) dims = rectangle_dimensions->to_vector();
	dims.insert(dims.begin(), size);

	// TODO: Is to() is blocking by default? This clone() is there to make sure
	// we don't pass control back to Haskell and free the data before we're
	// done reading it, but if to() already doesn't return until it's done
	// reading, then the clone is unnecessary.
	rectangle_values = torch::from_blob(values, dims, CPU_FLOAT).clone().to(torch::kCUDA);
	if(NULL != mask) masked_values = torch::from_blob(mask, dims, CPU_BYTE).clone().to(torch::kCUDA);
}

bool _endpoint::has_masks() const {
	auto ushape = shape;
	while(shape->tag == tag_masked) ushape = shape->child();
	switch(ushape->tag) {
		case tag_unit:
		case tag_positive:
		case tag_categorical:
			return masked_values.defined();
		case tag_rectangle:
			if(ushape->child()->is_leaf())
				return masked_values.defined();
			else {
				for(auto e: rectangle_children)
					if(!e->has_masks()) return false;
				return true;
			}
		case tag_heterogeneous:
			for(auto nm_child: heterogeneous_children)
				if(!nm_child.second->has_masks()) return false;
			return true;
	}

	cerr << "Invalid tag discovered in has_masks()" << endl;
	cerr << "Current endpoint: " << *this << endl;
	throw 0;
}

void free_endpoint_names(int size, char **names) {
	for(int i=0; i<size; ++i) free(names[i]);
	free(names);
}

void free_endpoint_values(float *values) { delete values; }
void free_endpoint(endpoint *e) { delete e; }

void dump_endpoint(endpoint *e) { flush(cout << *(e->ref)); }
ostream &operator<<(ostream &os, const _endpoint &e) {
	auto print_leaf = [&](structure_tag tag) {
		dump_leaf_type(os, tag) << " " << e.rectangle_values;
		if(e.masked_values.defined())
			os << '@' << e.masked_values;
	};

	switch(e.shape->tag) {
		case tag_unit:
		case tag_positive:
		case tag_categorical: print_leaf(e.shape->tag); return os;
		case tag_masked:
			{ auto e_child(e);
				e_child.shape = e.shape->child();
				return os << "!" << e_child;
			}
		case tag_rectangle:
			if(e.shape->child()->is_leaf()) print_leaf(e.shape->child()->tag);
			else {
				cerr << "<TODO: pretty-print complex rectangular values>";
				throw 0;
			}
			return os;
		case tag_heterogeneous:
			os << '{';
			{ auto it = e.heterogeneous_children.begin();
				if(it != e.heterogeneous_children.end()) while(true) {
					os << it->first << ": " << *it->second;
					++it;
					if(it == e.heterogeneous_children.end()) break;
					os << ", ";
				}
			}
			return os << '}';
	}

	cerr << "Invalid endpoint tag " << e.shape->tag << endl;
	throw 0;
}
