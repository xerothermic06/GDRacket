#include "util/scheme_classes.h"
#include "./racket_gdprimitive.h"
#include "./racket_builtin_binder.h"
#include "./racket_builtin_api.h"
#include "schexn.h"


// Procedure that just forwards to Godot's push_error.
Scheme_Object* _gdprimitive_push_error(int argc, Scheme_Object** argv) {
    if (SCHEME_CHAR_STRINGP(argv[0])) {
        UtilityFunctions::push_error(rktstr2gdstr(argv[0]));
    }
    // TODO: Racket exception
    return scheme_void;
}


// Procedure that gets an engine singleton. Delete this later when static method wrapper available for Engine.get_singleton
Scheme_Object* _gdprimitive_get_singleton(int argc, Scheme_Object** argv) {
    if (SCHEME_CHAR_STRINGP(argv[0])) {
        Object* obj = Engine::get_singleton()->get_singleton(rktstr2gdstrname(argv[0]));
        if (obj != nullptr) {
            return gd_obj2rkt_obj(&Variant(obj));
        }
    }
    // TODO: Racket exception
    return scheme_void;
}


// Procedure that checks if a Racket object is a Godot builtin wrapper.
Scheme_Object* _gdprimitive_gdobjectp(int argc, Scheme_Object** argv) {
    bool is_builtin = BuiltinBinder::get_singleton()->builtin_wrapperp(argv[0]);
    return is_builtin
        ? scheme_true
        : scheme_false;
}


void raise_scheme_exn_from_gde_call_error(GDExtensionCallErrorType errno) {
    if (errno == GDExtensionCallErrorType::GDEXTENSION_CALL_ERROR_INSTANCE_IS_NULL) {
        scheme_raise_exn(MZEXN_FAIL);
    }
}


// Procedure that calls a method on a builtin wrapper.
Scheme_Object* _gdprimitive_gd_variant_call(int argc, Scheme_Object** argv) {
    BuiltinBinder* converter = BuiltinBinder::get_singleton();
    if (!converter->builtin_wrapperp(argv[0])) {
        return scheme_void;
    }

    Variant obj = converter->scheme_object_to_variant(argv[0]);
    const StringName method_name = rktstr2gdstrname(argv[1]);

    Variant* args = nullptr;
    if (argc > 2) {
        args = new Variant[argc-2];
        for (int i = 0; i < argc - 2; i++) {
            args[i] = converter->scheme_object_to_variant(argv[i + 2]);
        }
    }
    Variant result;
    GDExtensionCallError err;
    obj.call(method_name, const_cast<const Variant**>(&args), argc - 2, result, err);
    if (err.error != GDExtensionCallErrorType::GDEXTENSION_CALL_OK) {
        String msg = String("Error calling method {0} on {1}: {2}").format(array_of(method_name, obj, err.error));
        scheme_raise_exn(MZEXN_FAIL, msg.utf8().get_data(), scheme_false);
        ERR_FAIL_MSG(String("Error calling method {0} on {1}: {2}").format(array_of(method_name, obj, err.error)));
    }
    return converter->variant_to_scheme_object(result);
}


// Procedure that sets an integer-based property from a builtin
Scheme_Object* _gdprimitive_gd_variant_seti(int argc, Scheme_Object** argv) {
    BuiltinBinder* converter = BuiltinBinder::get_singleton();
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
Scheme_Object* _gdprimitive_gd_variant_geti(int argc, Scheme_Object** argv) {
    BuiltinBinder* converter = BuiltinBinder::get_singleton();
    if (!converter->builtin_wrapperp(argv[0])
        || !(SCHEME_INTP(argv[1]))) {
        return scheme_void;
    }

    Variant vrnt = converter->scheme_object_to_variant(argv[0]);

    Variant::Type type = vrnt.get_type();

    int64_t idx = SCHEME_INT_VAL(argv[1]);
    Variant valu = rkt_obj2gd_obj(argv[2]);

    bool valid, oob;
    Variant res = vrnt.get_indexed(idx, valid, oob);
    return converter->variant_to_scheme_object(res);
}


#define rkt_add_prim_module_proc(nm, fn, modul, minargs, maxargs, returns) \
    scheme_add_global(nm, scheme_make_prim_w_everything(fn, 1, nm, minargs, maxargs, 0, returns, returns), modul)
Scheme_Env* gdprimitive_init_module(Scheme_Env* env) {
    Scheme_Env* modul = scheme_primitive_module(rkt_sym("gd-primitive"), env);

    rkt_add_prim_module_proc("push-error", _gdprimitive_push_error, modul, 1, 1, 0);
    rkt_add_prim_module_proc("get-singleton", _gdprimitive_get_singleton, modul, 1, 1, 1);
    rkt_add_prim_module_proc("object?", _gdprimitive_gdobjectp, modul, 1, 1, 1);
    rkt_add_prim_module_proc("call", _gdprimitive_gd_variant_call, modul, 2, 16, 1);
    rkt_add_prim_module_proc("geti", _gdprimitive_gd_variant_geti, modul, 2, 2, 1);
    rkt_add_prim_module_proc("seti!", _gdprimitive_gd_variant_seti, modul, 3, 3, 1);

    // Predicate functions for Scheme_Type structs.
    rkt_add_prim_module_proc("Vector2?",            gdobject_isp<Scheme_Vector2>,            modul, 1, 1, 1);
    rkt_add_prim_module_proc("Vector2i?",           gdobject_isp<Scheme_Vector2i>,           modul, 1, 1, 1);
    rkt_add_prim_module_proc("Rect2?",              gdobject_isp<Scheme_Rect2>,              modul, 1, 1, 1);
    rkt_add_prim_module_proc("Rect2i?",             gdobject_isp<Scheme_Rect2i>,             modul, 1, 1, 1);
    rkt_add_prim_module_proc("Vector3?",            gdobject_isp<Scheme_Vector3>,            modul, 1, 1, 1);
    rkt_add_prim_module_proc("Vector3i?",           gdobject_isp<Scheme_Vector3i>,           modul, 1, 1, 1);
    rkt_add_prim_module_proc("Transform2D?",        gdobject_isp<Scheme_Transform2D>,        modul, 1, 1, 1);
    rkt_add_prim_module_proc("Vector4?",            gdobject_isp<Scheme_Vector4>,            modul, 1, 1, 1);
    rkt_add_prim_module_proc("Vector4i?",           gdobject_isp<Scheme_Vector4i>,           modul, 1, 1, 1);
    rkt_add_prim_module_proc("Plane?",              gdobject_isp<Scheme_Plane>,              modul, 1, 1, 1);
    rkt_add_prim_module_proc("Quaternion?",         gdobject_isp<Scheme_Quaternion>,         modul, 1, 1, 1);
    rkt_add_prim_module_proc("AABB?",               gdobject_isp<Scheme_AABB>,               modul, 1, 1, 1);
    rkt_add_prim_module_proc("Basis?",              gdobject_isp<Scheme_Basis>,              modul, 1, 1, 1);
    rkt_add_prim_module_proc("Transform3D?",        gdobject_isp<Scheme_Transform3D>,        modul, 1, 1, 1);
    rkt_add_prim_module_proc("Projection?",         gdobject_isp<Scheme_Projection>,         modul, 1, 1, 1);
    rkt_add_prim_module_proc("Color?",              gdobject_isp<Scheme_Color>,              modul, 1, 1, 1);
    rkt_add_prim_module_proc("StringName?",         gdobject_isp<Scheme_StringName>,         modul, 1, 1, 1);
    rkt_add_prim_module_proc("NodePath?",           gdobject_isp<Scheme_NodePath>,           modul, 1, 1, 1);
    rkt_add_prim_module_proc("RID?",                gdobject_isp<Scheme_RID>,                modul, 1, 1, 1);
    rkt_add_prim_module_proc("Object?",             gdobject_isp<Scheme_GodotObject>,        modul, 1, 1, 1);
    rkt_add_prim_module_proc("Callable?",           gdobject_isp<Scheme_Callable>,           modul, 1, 1, 1);
    rkt_add_prim_module_proc("Signal?",             gdobject_isp<Scheme_Signal>,             modul, 1, 1, 1);
    rkt_add_prim_module_proc("Dictionary?",         gdobject_isp<Scheme_Dictionary>,         modul, 1, 1, 1);
    rkt_add_prim_module_proc("Array?",              gdobject_isp<Scheme_Array>,              modul, 1, 1, 1);
    rkt_add_prim_module_proc("PackedByteArray?",    gdobject_isp<Scheme_PackedByteArray>,    modul, 1, 1, 1);
    rkt_add_prim_module_proc("PackedInt32Array?",   gdobject_isp<Scheme_PackedInt32Array>,   modul, 1, 1, 1);
    rkt_add_prim_module_proc("PackedInt64Array?",   gdobject_isp<Scheme_PackedInt64Array>,   modul, 1, 1, 1);
    rkt_add_prim_module_proc("PackedFloat32Array?", gdobject_isp<Scheme_PackedFloat32Array>, modul, 1, 1, 1);
    rkt_add_prim_module_proc("PackedFloat64Array?", gdobject_isp<Scheme_PackedFloat64Array>, modul, 1, 1, 1);
    rkt_add_prim_module_proc("PackedStringArray?",  gdobject_isp<Scheme_PackedStringArray>,  modul, 1, 1, 1);
    rkt_add_prim_module_proc("PackedVector2Array?", gdobject_isp<Scheme_PackedVector2Array>, modul, 1, 1, 1);
    rkt_add_prim_module_proc("PackedVector3Array?", gdobject_isp<Scheme_PackedVector3Array>, modul, 1, 1, 1);
    rkt_add_prim_module_proc("PackedColorArray?",   gdobject_isp<Scheme_PackedColorArray>,   modul, 1, 1, 1);

    register_builtin_wrapper_ctors(modul);

    scheme_finish_primitive_module(modul);
    return modul;
}
