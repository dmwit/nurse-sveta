#pragma once

// This is kinda like the "public" API -- this is the one exported to Haskell.
// The .hpp version is kinda like the "private" API -- other C++ code is given superpowers.

// Don't sweat the small stuff on memory management, it's shared_ptr's all the
// way down. When you're done with a pointer given to you by this API, call the
// relevant free_* function and be on your merry way.

// Cyclic data is not supported, don't do it. Something will probably break.

// game_constants can be negative, in which case they're the ones complement
// (i.e. ~, not -) of a miscellaneous size; can't use enum for this reason.
// See also [NOTE: FFI, switch, and linking].
typedef int game_constant;
#define c_colors ((game_constant)0)
#define c_shapes ((game_constant)1)
#define c_width ((game_constant)2)
#define c_height ((game_constant)3)
#define c_orientations ((game_constant)4)

int eval_game_constant(game_constant c);
void dump_game_constant(game_constant c);

// I'm scared. What size is an enum, really? At least we know the size of an
// int for sure, which makes it easier to get the FFI bindings correct.
typedef int leaf_type;
// 0-1, squared-error loss
#define type_unit ((leaf_type)0)
// 0-∞, squared-error loss
#define type_positive ((leaf_type)1)
// positive, sums to 1, Kullback-Leibler divergence
#define type_categorical ((leaf_type)2)
// 0-1, cross-entropy loss
#define type_probability ((leaf_type)3)
// -∞-∞, squared-error loss
#define type_unbounded ((leaf_type)4)

void dump_leaf_type(leaf_type ty);

typedef int structure_tag;
#define tag_tensor ((structure_tag)0)
#define tag_vector ((structure_tag)1)
#define tag_dictionary ((structure_tag)2)
typedef struct structure structure;

structure *new_structure_tensor(leaf_type ty, int dim_count, game_constant *lens);
structure *new_structure_vector(game_constant len, structure *child);
structure *new_structure_dictionary();
void structure_add_child(structure *parent, char *name, structure *child);

void dump_structure(structure *s);
void free_structure(structure *s);

typedef struct endpoint endpoint;

// Indices vary slowest for the batch index, slower for earlier dimensions.
// mask may be NULL
endpoint *new_endpoint_tensor(int batch_size, int dim_count, game_constant *lens, float *values, char *mask);
endpoint *new_endpoint_vector(game_constant len, endpoint **es);
endpoint *new_endpoint_dictionary();
void endpoint_add_child(endpoint *parent, char *name, endpoint *child);

structure_tag endpoint_get_tag(endpoint *e);
// Indices vary in the same way as for new_endpoint_tensor.
void endpoint_read_tensor(int *ret_batch_size, int *ret_dim_count, game_constant **ret_lens, float **ret_values, char **ret_mask, endpoint *e);
void endpoint_read_vector(game_constant *ret_len, endpoint ***ret_children, endpoint *e);
void endpoint_read_dictionary(int *ret_count, char ***ret_names, endpoint ***ret_children, endpoint *parent);

void dump_endpoint(endpoint *e);
void free_endpoint_read_tensor_constants(game_constant *lens);
void free_endpoint_read_tensor_values(float *values);
void free_endpoint_read_tensor_mask(char *values);
void free_endpoint_read_vector(game_constant c, endpoint **children);
void free_endpoint_read_dictionary(int count, char **names, endpoint **children);
void free_endpoint(endpoint *e);
