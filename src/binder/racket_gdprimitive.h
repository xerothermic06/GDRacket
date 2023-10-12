#ifndef RACKET_GDPRIMITIVE_H
#define RACKET_GDPRIMITIVE_H

#include "./racket_binder_util.h"
#include "godot_cpp/variant/utility_functions.hpp"
#include "godot_cpp/classes/engine.hpp"
#include "godot_cpp/classes/object.hpp"

// Handmade Racket procedures that form the fully-native gdprimitive module.

using namespace godot;

#define GDPRIMITIVE_NEW_STRUCT_INSTANCE(TYPE, TAG) scheme_make_cptr(new (scheme_malloc(sizeof(TYPE))) TYPE(), rkt_sym(TAG))

// template <typename T, char* typetag>
// Scheme_Object* _gdprimitive_new_struct(int argc, Scheme_Object** argv) {
//     void* mem = scheme_malloc(sizeof(T));
//     T* def = new (mem) T();
//     return scheme_make_cptr(def, rkt_sym(name));
// }

// template <typename T, typename R>
// void set_struct_prop(T* obj, size_t offset, char* hash_key, Scheme_Object* hash_table, void (*fun)(Scheme_Object*)) {
//     Scheme_Object* value = scheme_hash_get(hash_table, rkt_sym(hash_key));
//     R converted = fun(value);
//     *(obj + offset) = converted;
// }


// template <typename T, char* typetag, StringSetterTable indexMap>
// Scheme_Object* _gdprimitive_struct_hash_unpack(int argc, Scheme_Object** argv) {
//     static StringSetterTable indexMap;

//     if (!(argc > 0 && SCHEME_HASHTP(arg[1]))) {
//         scheme_raise_exn(MZEXN_FAIL, "hash_unpack invalid arguments");
//     }

//     Scheme_Object* hash_table = argv[1];

//     void* mem = scheme_malloc(sizeof(T));
//     T* def = new (mem) T();
//     Scheme_Object cptr = scheme_make_cptr(def, rkt_sym(name));

//     for (HashMap<StringName, StructPropertySetter>::ConstIterator prop : indexMap->begin()) {
//         Scheme_Object* res = rkt_eval(rkt_sym("hash-ref"), gdstr2rktstr(prop->key));
//         prop->value()
//     }
// }



Scheme_Object* _gdprimitive_push_error(int argc, Scheme_Object** argv);
Scheme_Object* _gdprimitive_get_singleton(int argc, Scheme_Object** argv);
Scheme_Object* _gdprimitive_gdobjectp(int argc, Scheme_Object** argv);
// Scheme_Object* _gdprimitive_variantp(int argc, Scheme_Object** argv);
Scheme_Object* _gdprimitive_gd_variant_call(int argc, Scheme_Object** argv);
Scheme_Object* _gdprimitive_gd_variant_seti(int argc, Scheme_Object** argv);
Scheme_Object* _gdprimitive_gd_variant_geti(int argc, Scheme_Object** argv);
// Scheme_Object* _gdprimitive_gd_object_call(int argc, Scheme_Object** argv);

Scheme_Env* gdprimitive_init_module(Scheme_Env* env);

#endif // RACKET_GDPRIMITIVE_H
