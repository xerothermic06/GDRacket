#include "./racket_binder_util.h"

Scheme_Object* gd_obj2rkt_obj(const Variant* v) {
    static Scheme_Object* RKT_SYMBOL_OBJECT = rkt_sym("object");
    static Scheme_Object* RKT_SYMBOL_VARIANT = rkt_sym("variant");

    switch (v->get_type()) {
        case Variant::NIL:
            scheme_make_external_cptr((void*)v, RKT_SYMBOL_VARIANT);
        case Variant::BOOL:
            return (bool)v ? scheme_true : scheme_false;
        case Variant::INT:
            return scheme_make_integer((uint32_t)*v);
        case Variant::FLOAT:
            return scheme_make_float((float)*v);
        case Variant::STRING:
            return gdstr2rktstr(((String)*v));
        case Variant::STRING_NAME:
            return gdstrname2rktstr(((StringName)*v));
        case Variant::OBJECT:
            return scheme_make_external_cptr((void*)(Object*)*v, RKT_SYMBOL_OBJECT);
        default:
            // return scheme_make_external_cptr((void*)v, scheme_make_integer(v->get_type()));
            return scheme_make_external_cptr((void*)v, RKT_SYMBOL_VARIANT);
    }
    return scheme_void;
}


Variant rkt_obj2gd_obj(Scheme_Object* obj) {
    static Scheme_Object* RKT_SYMBOL_OBJECT = rkt_sym("object");
    static Scheme_Object* RKT_SYMBOL_VARIANT = rkt_sym("variant");

    if (SCHEME_BOOLP(obj)) { return Variant(SCHEME_TRUEP(obj)); }
    if (SCHEME_INTP(obj)) { return Variant(SCHEME_INT_VAL(obj)); }
    if (SCHEME_FLOATP(obj)) { return Variant(SCHEME_FLOAT_VAL(obj)); }
    if (SCHEME_CHAR_STRINGP(obj)) { return Variant(rktstr2gdstr(obj)); }
    if (SCHEME_CHAR_STRINGP(obj)) { return Variant(rktstr2gdstr(obj)); }
    if (SCHEME_CPTRP(obj)) {
        if (SCHEME_CPTR_TYPE(obj) == RKT_SYMBOL_OBJECT) {
            return Variant((Object*)SCHEME_CPTR_VAL(obj));
        }
        if (SCHEME_CPTR_TYPE(obj) == RKT_SYMBOL_VARIANT) {
            return *((Variant*)SCHEME_CPTR_VAL(obj));
        }
    }
    return Variant();
}


Scheme_Object* rkt_make_gd_string(int argc, Scheme_Object** argv) {
    if (argc < 1) {
        return scheme_void;
    }
    String* str = memnew(rktstr2gdstr(argv[0]));
    return scheme_make_cptr(str, rkt_sym("String"));
}


Scheme_Object* rkt_make_gd_stringname(int argc, Scheme_Object** argv) {
    if (argc < 1) {
        return scheme_void;
    }
    StringName* str = memnew(rktstr2gdstrname(argv[0]));
    return scheme_make_cptr(str, rkt_sym("StringName"));
}

