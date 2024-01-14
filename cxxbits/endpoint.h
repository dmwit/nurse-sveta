#pragma once

// This is kinda like the "public" API -- this is the one exported to Haskell.
// The .hpp version is kinda like the "private" API -- other C++ code is given superpowers.

// Don't sweat the small stuff on memory management, it's shared_ptr's all the
// way down. When you're done with a pointer given to you by this API, call the
// relevant free_* function and be on your merry way.

// Cyclic data is not supported, don't do it. Something will probably break.

typedef enum {
	c_colors,
	c_shapes,
	c_width,
	c_height,
	c_orientations,
	NUM_GAME_CONSTANTS
} game_constant;

int eval_game_constant(game_constant c);
void dump_game_constant(game_constant c);

typedef enum {
	tag_product,
	tag_sum,
	NUM_DIMENSIONS_TAGS
} dimensions_tag;

typedef struct dimensions dimensions;

dimensions *new_dimensions_product(int capacity_hint);
dimensions *new_dimensions_sum(int capacity_hint);
void dimensions_add_constant(dimensions *d, game_constant c);
dimensions_tag dimensions_get_tag(dimensions *d);
void dimensions_read(int *ret_length, game_constant **ret_constants, dimensions *d);

void dump_dimensions(dimensions *d);
// use when finished with the values returned by a dimensions_read
void free_dimensions_constants(game_constant *constants);
void free_dimensions(dimensions *d);

typedef enum {
	tag_unit, // 0-1, squared-error loss
	tag_positive, // 0-∞, squared-error loss
	tag_categorical, // 0-1, part of a collection that sums to 1, cross-entropy loss
	tag_masked, // only some entries are valid/should contribute to the loss
	tag_rectangle, // tensor
	tag_heterogeneous, // a mixed collection of types, specified as a dictionary with strings as keys
	NUM_STRUCTURE_TAGS
} structure_tag;

typedef struct structure structure;

structure *new_structure_unit();
structure *new_structure_positive();
structure *new_structure_categorical();
structure *new_structure_masked(structure *child);
structure *new_structure_rectangle(dimensions *d, structure *child);
structure *new_structure_heterogeneous();
void structure_add_child(structure *parent, char *name, structure *child);

void dump_structure(structure *s);
void free_structure(structure *s);

typedef struct endpoint endpoint;

// mask may be NULL
endpoint *new_endpoint_unit(int size, float *values, char *mask);
endpoint *new_endpoint_positive(int size, float *values, char *mask);
endpoint *new_endpoint_categorical(int size, float *values, char *mask);
endpoint *new_endpoint_masked(endpoint *child);
// This not fully general (it doesn't allow the creation of rectangular
// endpoints with non-leaf children), but let's delay making the API more
// complicated until we know we need it.
//
// Indices vary slowest for the batch index, slower for earlier product
// dimensions, and lower indices correspond to earlier sum dimensions.
// Currently only the unit, positive, and categorical tags are supported.
endpoint *new_endpoint_rectangle(int size, structure_tag child_type, dimensions *d, float *values, char *mask);
endpoint *new_endpoint_heterogeneous(int size);
void endpoint_add_child(endpoint *parent, char *name, endpoint *child);

structure_tag endpoint_get_tag(endpoint *e);
// only valid for heterogeneous endpoints
endpoint *endpoint_get_named_child(endpoint *parent, char *name);
// only valid for masked endpoints
endpoint *endpoint_get_masked_child(endpoint *parent);
// only valid for rectangle endpoints
dimensions *endpoint_get_dimensions(endpoint *e);
// valid for unit, positive, categorical, rectangle unit, rectangle positive,
// or rectangle categorical
//
// Indices vary in the same way as for new_endpoint_rectangle.
void endpoint_read(structure_tag *ret_tag, int *ret_size, float **ret_values, endpoint *e);

void dump_endpoint(endpoint *e);
// use when finished with the values returned by an endpoint_read
void free_endpoint_values(float *values);
void free_endpoint(endpoint *e);
