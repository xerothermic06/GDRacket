#include "binder/racket_builtin_binder.h"

#include "binder/racket_bc_runtime.h"
#include "binder/racket_builtin_binder.h"
#include "binder/racket_binder_util.h"

// Storage for Scheme_Type ids
Scheme_Type Scheme_Vector2::scheme_type = 0;
Scheme_Type Scheme_Vector2i::scheme_type = 0;
Scheme_Type Scheme_Rect2::scheme_type = 0;
Scheme_Type Scheme_Rect2i::scheme_type = 0;
Scheme_Type Scheme_Vector3::scheme_type = 0;
Scheme_Type Scheme_Vector3i::scheme_type = 0;
Scheme_Type Scheme_Transform2D::scheme_type = 0;
Scheme_Type Scheme_Vector4::scheme_type = 0;
Scheme_Type Scheme_Vector4i::scheme_type = 0;
Scheme_Type Scheme_Plane::scheme_type = 0;
Scheme_Type Scheme_Quaternion::scheme_type = 0;
Scheme_Type Scheme_AABB::scheme_type = 0;
Scheme_Type Scheme_Basis::scheme_type = 0;
Scheme_Type Scheme_Transform3D::scheme_type = 0;
Scheme_Type Scheme_Projection::scheme_type = 0;
Scheme_Type Scheme_Color::scheme_type = 0;
Scheme_Type Scheme_StringName::scheme_type = 0;
Scheme_Type Scheme_NodePath::scheme_type = 0;
Scheme_Type Scheme_RID::scheme_type = 0;
Scheme_Type Scheme_GodotObject::scheme_type = 0;
Scheme_Type Scheme_Callable::scheme_type = 0;
Scheme_Type Scheme_Signal::scheme_type = 0;
Scheme_Type Scheme_Dictionary::scheme_type = 0;
Scheme_Type Scheme_Array::scheme_type = 0;
Scheme_Type Scheme_PackedByteArray::scheme_type = 0;
Scheme_Type Scheme_PackedInt32Array::scheme_type = 0;
Scheme_Type Scheme_PackedInt64Array::scheme_type = 0;
Scheme_Type Scheme_PackedFloat32Array::scheme_type = 0;
Scheme_Type Scheme_PackedFloat64Array::scheme_type = 0;
Scheme_Type Scheme_PackedStringArray::scheme_type = 0;
Scheme_Type Scheme_PackedVector2Array::scheme_type = 0;
Scheme_Type Scheme_PackedVector3Array::scheme_type = 0;
Scheme_Type Scheme_PackedColorArray::scheme_type = 0;


RacketCallableCustomMethodPointer::RacketCallableCustomMethodPointer(Object* p_instance, Scheme_Object* p_proc) {
    instance = p_instance;
    procedure = p_proc;
}


RacketCallableCustomMethodPointer::~RacketCallableCustomMethodPointer() {
    procedure = nullptr;
}


void RacketCallableCustomMethodPointer::call(const Variant **p_arguments, int p_argcount, Variant &r_return_value, GDExtensionCallError &r_call_error) const {
    Scheme_Object* args_list = scheme_null;
    RacketBCRuntime* runtime = RacketBCRuntime::get_singleton();
    RacketBCBuiltinBinder* builtinBinder = RacketBCBuiltinBinder::get_singleton();
    Scheme_Object* res;
    bool ok;
    const Variant *v;

    for (int i = p_argcount - 1; i >= 0; i--) {
        v = p_arguments[i];
        args_list = scheme_make_pair(builtinBinder->variant_to_scheme_object(v), scheme_null);
    }

    runtime->eval_handle(scheme_make_pair(procedure, args_list), &res, &ok);
    if (ok) {
        r_return_value = builtinBinder->scheme_object_to_variant(res);
    }
}


RacketBCBuiltinBinder* RacketBCBuiltinBinder::singleton;
RacketBCBuiltinBinder* RacketBCBuiltinBinder::get_singleton() {
    if (singleton == nullptr) {
        singleton = new RacketBCBuiltinBinder();
    }
    return singleton;
}


#define _register_builtin(wrapper_name, type_name) wrapper_name::scheme_type = scheme_make_type(type_name);
// registers Scheme_Type IDs for each Godot builtin struct wrapper.
// TODO: figure out if there's a native printer/reader to hook into for these
// IMPORTANT: This must be called only once when the Racket runtime initializes.
void RacketBCBuiltinBinder::register_builtin_types() {
    _register_builtin(Scheme_Vector2,            "Vector2");
    _register_builtin(Scheme_Vector2i,           "Vector2i");
    _register_builtin(Scheme_Rect2,              "Rect2");
    _register_builtin(Scheme_Rect2i,             "Rect2i");
    _register_builtin(Scheme_Vector3,            "Vector3");
    _register_builtin(Scheme_Vector3i,           "Vector3i");
    _register_builtin(Scheme_Transform2D,        "Transform2D");
    _register_builtin(Scheme_Vector4,            "Vector4");
    _register_builtin(Scheme_Vector4i,           "Vector4i");
    _register_builtin(Scheme_Plane,              "Plane");
    _register_builtin(Scheme_Quaternion,         "Quaternion");
    _register_builtin(Scheme_AABB,               "AABB");
    _register_builtin(Scheme_Basis,              "Basis");
    _register_builtin(Scheme_Transform3D,        "Transform3D");
    _register_builtin(Scheme_Projection,         "Projection");
    _register_builtin(Scheme_Color,              "Color");
    _register_builtin(Scheme_StringName,         "StringName");
    _register_builtin(Scheme_NodePath,           "NodePath");
    _register_builtin(Scheme_RID,                "RID");
    _register_builtin(Scheme_GodotObject,        "GodotObject");
    _register_builtin(Scheme_Callable,           "Callable");
    _register_builtin(Scheme_Signal,             "Signal");
    _register_builtin(Scheme_Dictionary,         "Dictionary");
    _register_builtin(Scheme_Array,              "Array");
    _register_builtin(Scheme_PackedByteArray,    "PackedByteArray");
    _register_builtin(Scheme_PackedInt32Array,   "PackedInt32Array");
    _register_builtin(Scheme_PackedInt64Array,   "PackedInt64Array");
    _register_builtin(Scheme_PackedFloat32Array, "PackedFloat32Array");
    _register_builtin(Scheme_PackedFloat64Array, "PackedFloat64Array");
    _register_builtin(Scheme_PackedStringArray,  "PackedStringArray");
    _register_builtin(Scheme_PackedVector2Array, "PackedVector2Array");
    _register_builtin(Scheme_PackedVector3Array, "PackedVector3Array");
    _register_builtin(Scheme_PackedColorArray,   "PackedColorArray");

    builtin_scheme_type_min = Scheme_Vector2::scheme_type;
    builtin_scheme_type_max = Scheme_PackedColorArray::scheme_type;
}


Scheme_Object* RacketBCBuiltinBinder::variant_to_scheme_object(const godot::Variant &v) {
    Variant::Type vtype = v.get_type();
    _debug_logln("{0}", vtype);
    VariantToSchemeObject convFunc = variant_to_scheme_object_funcs[vtype];
    return convFunc(v);
}


Variant::Type RacketBCBuiltinBinder::get_variant_type(Scheme_Object* obj) {
    if (SCHEME_CHAR_STRINGP(obj)) {
        return Variant::Type::STRING;
    }
    if (SCHEME_SYMBOLP(obj)) {
        return Variant::Type::STRING_NAME;
    }
    if (SCHEME_INTP(obj)) {
        return Variant::Type::INT;
    }
    if (SCHEME_FLOATP(obj)) {
        return Variant::Type::FLOAT;
    }
    if (SCHEME_BOOLP(obj)) {
        return Variant::Type::BOOL;
    }
    if (builtin_wrapperp(obj)) {
        // TODO: this feels kind of dangerous; what if Scheme_GDObject_Header isn't
        // the first bytes of the struct?
        return (Variant::Type)((Scheme_GDObject_Header*)(obj))->variant_type;
    }
    return Variant::Type::NIL;
}


Variant RacketBCBuiltinBinder::scheme_object_to_variant(Scheme_Object* obj) {
    Variant::Type typ = get_variant_type(obj);
    return this->scheme_object_to_variant_funcs[typ](obj);
}


bool RacketBCBuiltinBinder::builtin_wrapperp(Scheme_Object* obj) {
    Scheme_Type obj_type = SCHEME_TYPE(obj);
    return obj_type >= builtin_scheme_type_min && obj_type <= builtin_scheme_type_max;
}
