#ifndef RACKET_BINDER_UTIL_H
#define RACKET_BINDER_UTIL_H

#include <stdio.h>
#include <iostream>

#include "scheme.h"
#include "godot_cpp/variant/variant.hpp"
#include "godot_cpp/core/memory.hpp"

// Binder utilities.

using namespace godot;


template<typename... T>
Array array_of(T... ts) {
    Array arr;
    const int size = sizeof...(ts);
    static_assert(size > 0);
    Variant els[size] = { ts... };
    for (int i = 0; i < size; i++) {
        arr.append(els[i]);
    }
    return arr;
}


template <typename... T>
void _debug_logln(String msg, T... ts) {
    const int size = sizeof...(ts);
    if (size == 0) {
        std::cout << msg.utf8().get_data() << std::endl;
    } else {
        Array arr = array_of(ts...);
        std::cout << msg.format(arr).utf8().get_data() << std::endl;
    }
}

// Builds a list of Racket objects from varargs
template<typename... T>
Scheme_Object* rkt_list(T... ts) {
    const int size = sizeof...(ts);
    static_assert(size > 0);
    Scheme_Object* rest[size] = {ts...};
    Scheme_Object* list = scheme_build_list(size, rest);
    return list;
}

#define rkt_int(x) scheme_make_integer_value(x)
// Get interned symbol
#define rkt_sym(x) scheme_intern_symbol(x)
// Quote a Scheme_Object*
#define rkt_quote(x) rkt_list(rkt_sym("quote"), x)
#define rkt_quote_sym(x) rkt_quote(rkt_sym("quote"))
// Create an (obj . obj) pair
#define rkt_pair(x, y) scheme_make_pair(x, y)
#define rkt_keyword(x) scheme_intern_exact_keyword(x, sizeof(x) - 1)
#define rkt_string(x) scheme_make_utf8_string(x)
// Make a Racket UTF-8 string from a Godot string
#define gdstr2rktstr(x) scheme_make_utf8_string(x.utf8().get_data())
#define gdstrname2rktstr(x) scheme_make_utf8_string(String(x).utf8().get_data())
#define gdstrname2rktsym(x) scheme_intern_symbol(String(x).utf8().get_data())
#define gdstr2charp(x) (x.utf8().get_data())

// TODO: figure out how to make this work if some platform has an `unsigned int`
// that isn't exactly a uint32_t
#define rktstr2gdstr(x) String((const char32_t*)SCHEME_CHAR_STR_VAL(x))
#define rktstr2gdstrname(x) StringName((const char32_t*)SCHEME_CHAR_STR_VAL(x))
#define rktsym2gdstrname(x) StringName(SCHEME_SYM_VAL(x))
#define rkt_eval(...) scheme_eval(rkt_list(__VA_ARGS__), root_scheme_env)
#define rkt_eval_string(str) scheme_eval_string(str, root_scheme_env)
#define rkt_eval_handle(...) scheme_eval(rkt_list(rkt_sym("eval-handle"), rkt_quote(rkt_list(__VA_ARGS__))), root_scheme_env)

Scheme_Object* gd_obj2rkt_obj(const Variant* v);
Variant rkt_obj2gd_obj(Scheme_Object* obj);
Scheme_Object* rkt_make_gd_string(int argc, Scheme_Object** argv);
Scheme_Object* rkt_make_gd_stringname(int argc, Scheme_Object** argv);

#endif // RACKET_BINDER_UTIL
