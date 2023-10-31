#include "./racket_gdprimitive.h"

#include "./racket_builtin_api.h"
#include "./racket_builtin_binder.h"
#include "binder/racket_bc_binder.h"
#include "schexn.h"
#include "util/racket_classes.h"

// Procedure that just forwards to Godot's push_error.
Scheme_Object *_gdprimitive_push_error(int argc, Scheme_Object **argv) {
    if (SCHEME_CHAR_STRINGP(argv[0])) {
        UtilityFunctions::push_error(RacketBCBuiltinBinder::get_singleton()->scheme_object_to_variant(argv[0]));
    }
    // TODO: Racket exception
    return scheme_void;
}


// Procedure that gets an engine singleton. Delete this later when static method wrapper available for
// Engine.get_singleton
Scheme_Object *_gdprimitive_get_singleton(int argc, Scheme_Object **argv) {
    if (SCHEME_SYMBOLP(argv[0])) {
        Object *obj = Engine::get_singleton()->get_singleton(rktsym2gdstrname(argv[0]));
        if (obj != nullptr) {
            return RacketBCBuiltinBinder::get_singleton()->variant_to_scheme_object(obj);
        }
    }
    // TODO: Racket exception
    return scheme_void;
}


// Procedure that checks if a Racket object is a Godot builtin wrapper.
Scheme_Object *_gdprimitive_gdobjectp(int argc, Scheme_Object **argv) {
    bool is_builtin = RacketBCBuiltinBinder::get_singleton()->builtin_wrapperp(argv[0]);
    return is_builtin ? scheme_true : scheme_false;
}


void raise_scheme_exn_from_gde_call_error(GDExtensionCallErrorType errno) {
    if (errno == GDExtensionCallErrorType::GDEXTENSION_CALL_ERROR_INSTANCE_IS_NULL) {
        scheme_raise_exn(MZEXN_FAIL);
    }
}


// Procedure that calls a method on a builtin wrapper.
Scheme_Object *_gdprimitive_gd_variant_call(int argc, Scheme_Object **argv) {
    RacketBCBuiltinBinder *converter = RacketBCBuiltinBinder::get_singleton();
    if (!converter->builtin_wrapperp(argv[0])) {
        return scheme_void;
    }

    // Convert object and method name
    Variant obj = converter->scheme_object_to_variant(argv[0]);
    const StringName method_name = converter->scheme_object_to_variant(argv[1]);

    // Convert function argument list to Variant list
    Variant *vargs = nullptr;
    Variant **varg_ptrs = nullptr;
    int list_length = scheme_list_length(argv[2]);
    if (list_length != 0) {
        Scheme_Object *cur_list = argv[2];
        vargs = new Variant[list_length];
        varg_ptrs = new Variant *[list_length];
        int i = 0;
        while (!SCHEME_NULLP(cur_list)) {
            Scheme_Object *arg_object = SCHEME_CAR(cur_list);
            vargs[i] = converter->scheme_object_to_variant(arg_object);
            varg_ptrs[i] = &vargs[i];
            cur_list = SCHEME_CDR(cur_list);
            i++;
        }
    }

    Variant result = Variant();
    GDExtensionCallError err = GDExtensionCallError{ GDExtensionCallErrorType::GDEXTENSION_CALL_OK, 0, 0 };
    obj.callp(method_name, const_cast<const Variant **>(varg_ptrs), list_length, result, err);

    if (err.error != GDExtensionCallErrorType::GDEXTENSION_CALL_OK) {
        String msg = String("Error calling method {0} on {1}: {2}").format(array_of(method_name, obj, err.error));
        scheme_raise_exn(MZEXN_FAIL, msg.utf8().get_data(), scheme_false);
        ERR_FAIL_V_MSG(scheme_null,
                String("Error calling method {0} on {1}: {2}").format(array_of(method_name, obj, err.error)));
    }
    return converter->variant_to_scheme_object(result);
}


// Procedure that sets an integer-based property from a builtin
Scheme_Object *_gdprimitive_gd_variant_seti(int argc, Scheme_Object **argv) {
    RacketBCBuiltinBinder *converter = RacketBCBuiltinBinder::get_singleton();
    if (!converter->builtin_wrapperp(argv[0]) || !(SCHEME_INTP(argv[1]))) {
        return scheme_false;
    }
    Variant obj = converter->scheme_object_to_variant(argv[0]);
    int64_t idx = SCHEME_INT_VAL(argv[1]);
    Variant valu = converter->scheme_object_to_variant(argv[2]);
    bool valid, oob;
    obj.set_indexed(idx, valu, valid, oob);
    return valid && !oob ? scheme_true : scheme_false;
}


// Procedure that gets an integer-based property from a builtin
Scheme_Object *_gdprimitive_gd_variant_geti(int argc, Scheme_Object **argv) {
    RacketBCBuiltinBinder *converter = RacketBCBuiltinBinder::get_singleton();
    if (!converter->builtin_wrapperp(argv[0]) || !(SCHEME_INTP(argv[1]))) {
        return scheme_void;
    }

    Variant vrnt = converter->scheme_object_to_variant(argv[0]);

    Variant::Type type = vrnt.get_type();

    int64_t idx = SCHEME_INT_VAL(argv[1]);
    Variant valu = converter->scheme_object_to_variant(argv[2]);

    bool valid, oob;
    Variant res = vrnt.get_indexed(idx, valid, oob);
    return converter->variant_to_scheme_object(res);
}


// Procedure that instantiates a Godot object for a Racket class instance when
// it is created from Racket code.
Scheme_Object *_gdprimitive_initialize_script_object(int argc,
                                                     Scheme_Object **argv) {
    return nullptr;
}


Scheme_Object *_gdprimitive_instantiate(int argc, Scheme_Object **argv) {
    RacketBCBuiltinBinder *binder = RacketBCBuiltinBinder::get_singleton();
    StringName cls_name = binder->scheme_object_to_variant(argv[0]);
    if (cls_name.is_empty()) {
        scheme_raise_exn(MZEXN_FAIL, "class name must be symbol", scheme_false);
    }
    if (!RacketClassDB::class_exists(cls_name)) {
        scheme_raise_exn(MZEXN_FAIL, "non-existent class", scheme_false);
    }
    return binder->variant_to_scheme_object(
        RacketClassDB::instantiate(cls_name));
}


Scheme_Object *_gdprimitive_get_api_class_info(int argc, Scheme_Object **argv) {
    if (!SCHEME_SYMBOLP(argv[0])) {
        return scheme_null;
    }
    RacketBCBuiltinBinder *builtin_binder = RacketBCBuiltinBinder::get_singleton();
    RacketBCBinder *racket_binder = RacketBCBinder::get_singleton();
    StringName class_name = builtin_binder->scheme_object_to_variant(argv[0]);
    if (class_name.is_empty()) {
        return scheme_null;
    }
    Scheme_Hash_Table *ht = scheme_make_hash_table_equal();
    scheme_hash_set(
            ht, rkt_sym("methods"), racket_binder->array_to_list(RacketClassDB::class_get_method_list(class_name, true)));
    scheme_hash_set(ht, rkt_sym("properties"),
            racket_binder->array_to_list(RacketClassDB::class_get_property_list(class_name, true)));
    scheme_hash_set(ht, rkt_sym("parent"),
            builtin_binder->variant_to_scheme_object(RacketClassDB::get_parent_class(class_name)));
    return (Scheme_Object *)ht;
}


Scheme_Object *_gdprimitive_builtin_predicate(int argc, Scheme_Object **argv) {
    RacketBCBuiltinBinder *binder = RacketBCBuiltinBinder::get_singleton();
    static Scheme_Object *res[] = { scheme_false, scheme_true };

    if (!SCHEME_INTP(argv[0]) || !binder->builtin_wrapperp(argv[1])) {
        return scheme_false;
    }

    return res[(Variant::Type)SCHEME_INT_VAL(argv[0]) == binder->get_variant_type(argv[1])];
}


Scheme_Object *_gdprimitive_builtin_construct(int argc, Scheme_Object **argv) {
    RacketBCBuiltinBinder *binder = RacketBCBuiltinBinder::get_singleton();
    int ctor_args = argc - 1;
    GDExtensionVariantType type = (GDExtensionVariantType)SCHEME_INT_VAL(argv[0]);
    GDExtensionVariantPtr base;
    // TODO: find maximum ctor arg count, slightly wasteful of stack space but
    // probably faster than doing malloc
    Variant vargs[8]; // stack space for variants
    GDExtensionConstVariantPtr args[8];
    GDExtensionCallError err;
    int arg_pos = 0;

    for (int i = 1; i < argc - 1; i++) {
        vargs[arg_pos] = binder->scheme_object_to_variant(argv[i]);
        args[arg_pos] = (GDExtensionConstVariantPtr)(vargs[arg_pos]);
        arg_pos += 1;
    }
    godot::internal::gdextension_interface_variant_construct(type, &base, args, ctor_args, &err);

    ERR_FAIL_COND_V_MSG(err.error != GDExtensionCallErrorType::GDEXTENSION_CALL_OK, scheme_null,
            _string_fmt("invalid constructor: {0}", err.error));

    return binder->variant_to_scheme_object(Variant((GDExtensionConstVariantPtr)base));
}


Scheme_Object *_gdprimitive_builtin_operator(int argc, Scheme_Object **argv) {
    RacketBCBuiltinBinder *binder = RacketBCBuiltinBinder::get_singleton();
    Scheme_Object *ret = scheme_null;
    Variant res = Variant(), operand_a = Variant(), operand_b = Variant();
    bool valid = false;
    Variant::Operator op;

    ERR_FAIL_COND_V_MSG(SCHEME_INTP(argv[0]), ret, "Operator not an int");

    op = (Variant::Operator)SCHEME_INT_VAL(argv[0]);
    ERR_FAIL_COND_V_MSG(op < Variant::Operator::OP_MAX && op >= Variant::Operator::OP_EQUAL, ret,
            _string_fmt("Operator invalid {0}", op));

    operand_a = binder->scheme_object_to_variant(argv[0]);
    if (argc > 2) {
        operand_b = binder->scheme_object_to_variant(argv[1]);
    }

    operand_a.evaluate(op, operand_a, operand_b, res, valid);

    ERR_FAIL_COND_V_MSG(!valid, ret, _string_fmt("Operator use invalid {0} {1} {2}", op, operand_a, operand_b));
    ret = binder->variant_to_scheme_object(res);
    return ret;
}


#define rkt_add_prim_module_proc(nm, fn, modul, minargs, maxargs, returns) \
    scheme_add_global(nm, scheme_make_prim_w_everything(fn, 1, nm, minargs, maxargs, 0, returns, returns), modul)
Scheme_Env *gdprimitive_init_module(Scheme_Env *env) {
    Scheme_Env *modul = scheme_primitive_module(rkt_sym("gd-primitive"), env);

    rkt_add_prim_module_proc("push-error", _gdprimitive_push_error, modul, 1, 1, 0);
    rkt_add_prim_module_proc("get-singleton", _gdprimitive_get_singleton, modul, 1, 1, 1);
    rkt_add_prim_module_proc("gdobject?", _gdprimitive_gdobjectp, modul, 1, 1, 1);
    rkt_add_prim_module_proc("call", _gdprimitive_gd_variant_call, modul, 3, 3, 1);
    rkt_add_prim_module_proc("geti", _gdprimitive_gd_variant_geti, modul, 2, 2, 1);
    rkt_add_prim_module_proc("seti!", _gdprimitive_gd_variant_seti, modul, 3, 3, 1);

    rkt_add_prim_module_proc("get-api-class-info", _gdprimitive_get_api_class_info, modul, 1, 1, 1);
    rkt_add_prim_module_proc("instantiate", _gdprimitive_instantiate, modul, 1, 1, 1);

    rkt_add_prim_module_proc("builtin-predicate", _gdprimitive_builtin_predicate, modul, 2, 2, 1);
    rkt_add_prim_module_proc("builtin-construct", _gdprimitive_builtin_construct, modul, 1, 9, 1);
    rkt_add_prim_module_proc("builtin-operator", _gdprimitive_builtin_operator, modul, 2, 3, 1);

    register_builtin_wrapper_ctors(modul);

    scheme_finish_primitive_module(modul);
    return modul;
}
