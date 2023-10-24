#ifndef RACKET_GDPRIMITIVE_H
#define RACKET_GDPRIMITIVE_H

#include "godot_cpp/variant/utility_functions.hpp"
#include "godot_cpp/classes/engine.hpp"
#include "godot_cpp/classes/object.hpp"

#include "binder/racket_binder_util.h"

// Handmade Racket procedures that form the fully-native gdprimitive module.

using namespace godot;

#define GDPRIMITIVE_NEW_STRUCT_INSTANCE(TYPE, TAG) scheme_make_cptr(new (scheme_malloc(sizeof(TYPE))) TYPE(), rkt_sym(TAG))

Scheme_Object* _gdprimitive_push_error(int argc, Scheme_Object** argv);
Scheme_Object* _gdprimitive_get_singleton(int argc, Scheme_Object** argv);
Scheme_Object* _gdprimitive_gdobjectp(int argc, Scheme_Object** argv);
Scheme_Object* _gdprimitive_gd_variant_call(int argc, Scheme_Object** argv);
Scheme_Object* _gdprimitive_gd_variant_seti(int argc, Scheme_Object** argv);
Scheme_Object* _gdprimitive_gd_variant_geti(int argc, Scheme_Object** argv);

Scheme_Env* gdprimitive_init_module(Scheme_Env* env);

#endif // RACKET_GDPRIMITIVE_H
