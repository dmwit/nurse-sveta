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
		default:
			if(c < 0) return ~c;
			cerr << "Invalid game constant " << c << endl;
			throw 0;
	}
}

int eval_game_constants(vector<game_constant> cs) {
	int v = 1;
	for(auto c: cs) v *= eval_game_constant(c);
	return v;
}

vector<int64_t> tensor_dimensions(int64_t batch_size, const vector<game_constant> &dims) {
	vector<int64_t> out;
	out.reserve(dims.size()+1);
	out.push_back(batch_size);
	transform(dims.begin(), dims.end(), back_inserter(out), eval_game_constant);
	return out;
}

void dump_game_constant(game_constant c) { flush(dump_game_constant(cout, c)); }
ostream &dump_game_constant(ostream &os, const game_constant c) {
	os << eval_game_constant(c);
	switch(c) {
		case c_colors: return os << "(colors)";
		case c_shapes: return os << "(shapes)";
		case c_width: return os << "(width)";
		case c_height: return os << "(height)";
		case c_orientations: return os << "(orientations)";
		default:
			if(c<0) return os;
			cerr << "Invalid game constant " << c << "in operator<<." << endl;
			throw 0;
	}
}

void dump_leaf_type(leaf_type ty) { flush(dump_leaf_type(cout, ty)); }
ostream &dump_leaf_type(ostream &os, const leaf_type ty) {
	switch(ty) {
		case type_unit: return os << "sigmoid/L2";
		case type_positive: return os << "exp/L2";
		case type_categorical: return os << "softmax/KL divergence";
		case type_probability: return os << "sigmoid/cross entropy";
		default:
			cerr << "Invalid leaf type " << ty << endl;
			throw 0;
	}
}

structure *new_structure_tensor(leaf_type ty, int dim_count, game_constant *lens) {
	_structure *s = new _structure();
	s->tag = tag_tensor;
	s->ty = ty;
	s->dims.reserve(dim_count);
	copy(lens, lens+dim_count, back_inserter(s->dims));
	return new structure(s);
}

structure *new_structure_vector(game_constant len, structure *child) {
	_structure *s = new _structure();
	s->tag = tag_vector;
	s->dims.push_back(len);
	s->vec = child->ref;
	return new structure(s);
}

structure *new_structure_dictionary() {
	_structure *s = new _structure();
	s->tag = tag_dictionary;
	return new structure(s);
}

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

	if(tag != tag_dictionary) die("Adding children is only allowed for dictionaries.");
	if(!dict.emplace(name, child).second)
		die("Attempted to add a child to a dictionary that already contained that name.");
}

void free_structure(structure *s) { delete s; }

ostream &dump_game_constants(ostream &os, const vector<game_constant> &gcs) {
	if(gcs.size() == 0) os << '-';
	else dump_game_constant(os, gcs[0]);
	for(int i = 1; i < gcs.size(); ++i)
		dump_game_constant(os << 'x', gcs[i]);
	return os;
}

void dump_structure(structure *s) { flush(cout << *(s->ref)); }
ostream &operator<<(ostream &os, const _structure &s) {
	switch(s.tag) {
		case tag_tensor:
			return dump_leaf_type(dump_game_constants(os, s.dims) << ' ', s.ty);
		case tag_vector:
			return dump_game_constant(os, s.dims[0]) << ' ' << *s.vec;
		case tag_dictionary:
			os << '{';
			{ auto it(s.dict.begin());
				if(it != s.dict.end()) while(true) {
					// TODO: escape it->first in some sensible way
					os << it->first << ": " << *it->second;
					++it;
					if(it == s.dict.end()) break;
					os << ", ";
				}
			}
			return os << '}';
		default:
			cerr << "Invalid structure tag " << s.tag << endl;
			throw 0;
	}
}

endpoint *new_endpoint_tensor(int batch_size, int dim_count, game_constant *lens, float *values, char *mask) {
	_endpoint *e = new _endpoint();
	e->size = batch_size;
	e->tag = tag_tensor;
	copy(lens, lens+dim_count, back_inserter(e->dims));
	auto dims = tensor_dimensions(batch_size, e->dims);

	// TODO: Is to() is blocking by default? This clone() is there to make sure
	// we don't pass control back to Haskell and free the data before we're
	// done reading it, but if to() already doesn't return until it's done
	// reading, then the clone is unnecessary.
	e->values = torch::from_blob(values, dims, CPU_FLOAT).clone().to(torch::kCUDA);
	if(NULL != mask) e->mask = torch::from_blob(mask, dims, CPU_BYTE).clone().to(torch::kCUDA);

	return new endpoint(e);
}

// TODO: we should assert that all the children have the same shape
endpoint *new_endpoint_vector(game_constant len, endpoint **es) {
	_endpoint *e = new _endpoint();
	e->size = -1;
	e->tag = tag_vector;
	e->dims.push_back(len);
	const int n = eval_game_constant(len);

	for(int i = 0; i < n; i++) {
		auto child = es[i]->ref;
		auto die = [&](string err) {
			cerr << err << endl;
			cerr << "Parent size: " << e->size << endl;
			cerr << "Child size: " << child->size << endl;
			cerr << "Parent: " << *e << endl;
			cerr << "Child: " << *child << endl;
			throw 0;
		};

		if(!e->assert_size(child->size)) die("Tried to make a vector endpoint with children of differing batch sizes.");
		// do not delete this line as an optimization after everything is
		// working; we want the modifications it potentially does to the child
		// to happen
		if(!child->assert_size(e->size)) die("The impossible happened: a parent's size was compatible with its child's, but not vice versa. This is likely a bug in assert_size, not in the caller of new_endpoint_vector.");
		e->vec.push_back(child);
	}

	return new endpoint(e);
}

endpoint *new_endpoint_dictionary() {
	_endpoint *e = new _endpoint();
	e->size = -1;
	e->tag = tag_dictionary;
	return new endpoint(e);
}

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

	if(tag != tag_dictionary) die("Adding children is only allowed for dictionaries.");
	if(!assert_size(child->size)) die("Parent endpoint and child endpoint have differing batch sizes (" + to_string(size) + " in parent, " + to_string(child->size) + " in child).");
	// do not delete this line as an optimization after everything is working;
	// we want the modifications it potentially does to the child to happen
	if(!child->assert_size(size)) die("The impossible happened: a parent's size was compatible with its child's, but not vice versa. This is likely a bug in assert_size, not in the caller of _endpoint::add_child.");
	if(!dict.emplace(name, child).second) die("Attempted to add a child to a dictionary that already contained that name.");
}

bool _endpoint::assert_size(int other_size) {
	if(other_size < 0) return true;
	if(size < 0) {
		size = other_size;
		switch(tag) {
			case tag_tensor: return true;
			case tag_vector:
				return all_of(vec.begin(), vec.end(), [&](auto child) { return child->assert_size(other_size); });
			case tag_dictionary:
				return all_of(dict.begin(), dict.end(), [&](auto child) { return child.second->assert_size(other_size); });
			default:
				cerr << "Invalid tag found in assert_size: " << tag << endl;
				throw 0;
		}
	}
	return size == other_size;
}

structure_tag endpoint_get_tag(endpoint *e) { return e->ref->tag; }

void endpoint_read_tensor(int *ret_batch_size, int *ret_dim_count, game_constant **ret_lens, float **ret_values, char **ret_mask, endpoint *_e) {
	auto e = _e->ref;
	if(e->tag != tag_tensor) {
		cerr << "Reading tensor contents is only allowed for tensor endpoints." << endl;
		cerr << "Current endpoint: " << *e << endl;
		throw 0;
	}

	*ret_batch_size = max(0, e->size);
	*ret_dim_count = e->dims.size();
	*ret_lens = new game_constant[*ret_dim_count];
	copy(e->dims.begin(), e->dims.end(), *ret_lens);

	int len = *ret_batch_size * eval_game_constants(e->dims);
	*ret_values = new float[len];
	auto contiguous = e->values.to(torch::kCPU).contiguous();
	copy(contiguous.data_ptr<float>(), contiguous.data_ptr<float>() + len, *ret_values);

	if(e->mask.defined()) {
		*ret_mask = new char[len];
		contiguous = e->mask.to(torch::kCPU).contiguous();
		copy(contiguous.data_ptr<unsigned char>(), contiguous.data_ptr<unsigned char>() + len, *ret_mask);
	} else {
		*ret_mask = nullptr;
	}
}

void endpoint_read_vector(game_constant *ret_len, endpoint ***ret_children, endpoint *_e) {
	auto e = _e->ref;
	if(e->tag != tag_vector) {
		cerr << "Accessing recursive children is only allowed for vector endpoints." << endl;
		cerr << "Current endpoint: " << *e << endl;
		throw 0;
	}

	*ret_len = e->dims[0];
	const int len_val = eval_game_constant(*ret_len);
	*ret_children = new endpoint *[len_val];
	for(int i = 0; i < len_val; i++) {
		(*ret_children)[i] = new endpoint(e->vec[i]);
	}
}

void endpoint_read_dictionary(int *ret_count, char ***ret_names, endpoint ***ret_children, endpoint *_parent) {
	auto parent = _parent->ref;
	if(parent->tag != tag_dictionary) {
		cerr << "Accessing named children is only allowed for dictionary endpoints." << endl;
		cerr << "Current endpoint: " << *parent << endl;
		throw 0;
	}

	*ret_count = parent->dict.size();
	*ret_names = new char *[*ret_count];
	*ret_children = new endpoint *[*ret_count];

	int i = 0;
	for(auto pair: parent->dict) {
		(*ret_names)[i] = new char[pair.first.size()+1];
		copy(pair.first.begin(), pair.first.end(), (*ret_names)[i]);
		(*ret_names)[i][pair.first.size()] = 0;
		(*ret_children)[i] = new endpoint(pair.second);
		++i;
	}
}

void free_endpoint_read_tensor_constants(game_constant *lens) { delete lens; }
void free_endpoint_read_tensor_values(float *values) { delete values; }
void free_endpoint_read_tensor_mask(char *mask) { delete mask; }
void free_endpoint_read_vector(game_constant c, endpoint **children) {
	const int len = eval_game_constant(c);
	for(int i = 0; i < len; i++) delete children[i];
	delete children;
}
void free_endpoint_read_dictionary(int count, char **names, endpoint **children) {
	for(int i = 0; i < count; i++) {
		delete names[i];
		delete children[i];
	}
	delete names;
	delete children;
}
void free_endpoint(endpoint *e) { delete e; }

void dump_endpoint(endpoint *e) { flush(cout << *(e->ref)); }
ostream &operator<<(ostream &os, const _endpoint &e) {
	switch(e.tag) {
		case tag_tensor:
			os << e.values;
			if(e.mask.defined()) os << '@' << e.mask;
			return os;
		case tag_vector:
			os << '[';
			if(e.vec.size() > 0) os << *e.vec[0];
			for(int i = 1; i < e.vec.size(); i++) {
				os << ", " << *e.vec[i];
			}
			return os << ']';
		case tag_dictionary:
			os << '{';
			{ auto it = e.dict.begin();
				if(it != e.dict.end()) while(true) {
					os << it->first << ": " << *it->second;
					++it;
					if(it == e.dict.end()) break;
					os << ", ";
				}
			}
			return os << '}';
	}

	cerr << "Invalid endpoint tag " << e.tag << endl;
	throw 0;
}
