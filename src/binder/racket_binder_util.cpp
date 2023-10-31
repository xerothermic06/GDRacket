#include "./racket_binder_util.h"


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

