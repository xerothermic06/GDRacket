#ifndef RACKET_BUILTIN_BINDER_H
#define RACKET_BUILTIN_BINDER_H

#include "binder/racket_binder_util.h"

#include "scheme.h"

#include "godot_cpp/templates/hash_map.hpp"
#include "godot_cpp/variant/variant.hpp"
#include "godot_cpp/core/object.hpp"

// Type definitions and utilities for binding Godot builtin structs to Racket
// wrapper values.

using namespace godot;

// Scheme_Object-compatible structs that wrap builtin Godot struct types.

// First section of builtin wrapper structs. Stores both Racket and Godot type tags.
typedef struct Scheme_GDObject_Header {
    Scheme_Object so;

    // TODO: see if there's a better way to correlate the Scheme_Type of this
    // struct with the corresponding Variant::Type number. Not an issue for now
    // because the last 2 bytes of this struct are available, but will be an
    // issue if more flags are needed
    unsigned short variant_type : 6;

} Scheme_GDObject_Header;


// Generates builtin wrapper types.
template <typename T, Variant::Type vtype>
struct Scheme_GDObject {

    Scheme_GDObject_Header header;
    // builtin struct value
    T v;

    // Static version of Variant::Type association
    static const Variant::Type wrapper_variant_type = vtype;
    static Scheme_Type scheme_type;

};


// generates predicates to check if given Scheme_Object is an instance of a builtin type
template <typename T>
Scheme_Object* gdobject_isp(int argc, Scheme_Object** argv) {
    return sizeof(*argv[0]) == sizeof(T) && ((T*)argv[0])->header.variant_type == T::wrapper_variant_type ? scheme_true : scheme_false; // SCHEME_TYPE(argv[0]) == T::type_id ? scheme_true : scheme_false;
}


// generates functions to convert Variant -> Scheme_Object
template<typename GDObjectType, typename BuiltinType>
Scheme_Object* variant_to_wrapper(const Variant& arg) {
    GDObjectType* scheme_gd_object = BuiltinBinder::get_singleton()->new_builtin_instance<GDObjectType>();
    scheme_gd_object->v = (BuiltinType)arg;
    return reinterpret_cast<Scheme_Object*>(scheme_gd_object);
}


// generates functions to convert Scheme_Object -> Variant
template<typename GDObjectType>
Variant wrapper_to_variant(Scheme_Object* obj) {
    return Variant(((GDObjectType*)obj)->v);
}


typedef struct Scheme_Vector2            : Scheme_GDObject<Vector2,            Variant::Type::VECTOR2>              {} Scheme_Vector2;
typedef struct Scheme_Vector2i           : Scheme_GDObject<Vector2i,           Variant::Type::VECTOR2I>             {} Scheme_Vector2i;
typedef struct Scheme_Rect2              : Scheme_GDObject<Rect2,              Variant::Type::RECT2>                {} Scheme_Rect2;
typedef struct Scheme_Rect2i             : Scheme_GDObject<Rect2i,             Variant::Type::RECT2I>               {} Scheme_Rect2i;
typedef struct Scheme_Vector3            : Scheme_GDObject<Vector3,            Variant::Type::VECTOR3>              {} Scheme_Vector3;
typedef struct Scheme_Vector3i           : Scheme_GDObject<Vector3i,           Variant::Type::VECTOR3I>             {} Scheme_Vector3i;
typedef struct Scheme_Transform2D        : Scheme_GDObject<Transform2D,        Variant::Type::TRANSFORM2D>          {} Scheme_Transform2D;
typedef struct Scheme_Vector4            : Scheme_GDObject<Vector4,            Variant::Type::VECTOR4>              {} Scheme_Vector4;
typedef struct Scheme_Vector4i           : Scheme_GDObject<Vector4i,           Variant::Type::VECTOR4I>             {} Scheme_Vector4i;
typedef struct Scheme_Plane              : Scheme_GDObject<Plane,              Variant::Type::PLANE>                {} Scheme_Plane;
typedef struct Scheme_Quaternion         : Scheme_GDObject<Quaternion,         Variant::Type::QUATERNION>           {} Scheme_Quaternion;
typedef struct Scheme_AABB               : Scheme_GDObject<AABB,               Variant::Type::AABB>                 {} Scheme_AABB;
typedef struct Scheme_Basis              : Scheme_GDObject<Basis,              Variant::Type::BASIS>                {} Scheme_Basis;
typedef struct Scheme_Transform3D        : Scheme_GDObject<Transform3D,        Variant::Type::TRANSFORM3D>          {} Scheme_Transform3D;
typedef struct Scheme_Projection         : Scheme_GDObject<Projection,         Variant::Type::PROJECTION>           {} Scheme_Projection;
typedef struct Scheme_Color              : Scheme_GDObject<Color,              Variant::Type::COLOR>                {} Scheme_Color;
typedef struct Scheme_StringName         : Scheme_GDObject<StringName,         Variant::Type::STRING_NAME>          {} Scheme_StringName;
typedef struct Scheme_NodePath           : Scheme_GDObject<NodePath,           Variant::Type::NODE_PATH>            {} Scheme_NodePath;
typedef struct Scheme_RID                : Scheme_GDObject<RID,                Variant::Type::RID>                  {} Scheme_RID;
typedef struct Scheme_GodotObject        : Scheme_GDObject<Object*,            Variant::Type::OBJECT>               {} Scheme_GodotObject;
typedef struct Scheme_Callable           : Scheme_GDObject<Callable,           Variant::Type::CALLABLE>             {} Scheme_Callable;
typedef struct Scheme_Signal             : Scheme_GDObject<Signal,             Variant::Type::SIGNAL>               {} Scheme_Signal;
typedef struct Scheme_Dictionary         : Scheme_GDObject<Dictionary,         Variant::Type::DICTIONARY>           {} Scheme_Dictionary;
typedef struct Scheme_Array              : Scheme_GDObject<Array,              Variant::Type::ARRAY>                {} Scheme_Array;
typedef struct Scheme_PackedByteArray    : Scheme_GDObject<PackedByteArray,    Variant::Type::PACKED_BYTE_ARRAY>    {} Scheme_PackedByteArray;
typedef struct Scheme_PackedInt32Array   : Scheme_GDObject<PackedInt32Array,   Variant::Type::PACKED_INT32_ARRAY>   {} Scheme_PackedInt32Array;
typedef struct Scheme_PackedInt64Array   : Scheme_GDObject<PackedInt64Array,   Variant::Type::PACKED_INT64_ARRAY>   {} Scheme_PackedInt64Array;
typedef struct Scheme_PackedFloat32Array : Scheme_GDObject<PackedFloat32Array, Variant::Type::PACKED_FLOAT32_ARRAY> {} Scheme_PackedFloat32Array;
typedef struct Scheme_PackedFloat64Array : Scheme_GDObject<PackedFloat64Array, Variant::Type::PACKED_FLOAT64_ARRAY> {} Scheme_PackedFloat64Array;
typedef struct Scheme_PackedStringArray  : Scheme_GDObject<PackedStringArray,  Variant::Type::PACKED_STRING_ARRAY>  {} Scheme_PackedStringArray;
typedef struct Scheme_PackedVector2Array : Scheme_GDObject<PackedVector2Array, Variant::Type::PACKED_VECTOR2_ARRAY> {} Scheme_PackedVector2Array;
typedef struct Scheme_PackedVector3Array : Scheme_GDObject<PackedVector3Array, Variant::Type::PACKED_VECTOR3_ARRAY> {} Scheme_PackedVector3Array;
typedef struct Scheme_PackedColorArray   : Scheme_GDObject<PackedColorArray,   Variant::Type::PACKED_COLOR_ARRAY>   {} Scheme_PackedColorArray;


typedef Scheme_Object* (*BuiltinToWrapper)(const Variant& v);
typedef Variant (*WrapperToBuiltin)(Scheme_Object* v);


// Singleton that converts between builtin Godot struct values and Racket values.
class BuiltinBinder {
private:
    static BuiltinBinder* singleton;

    // Scheme_Type range associated with builtin wrappers.
    Scheme_Type builtin_scheme_type_min;
    Scheme_Type builtin_scheme_type_max;

    // Functions to convert from builtin wrappers to Variants.
    WrapperToBuiltin wrapper_to_builtin_funcs[Variant::Type::VARIANT_MAX] = {
        [](Scheme_Object* obj)->Variant { return Variant(); },
        [](Scheme_Object* obj)->Variant { return Variant(SCHEME_TRUEP(obj)); },
        [](Scheme_Object* obj)->Variant { return Variant(SCHEME_INT_VAL(obj)); },
        [](Scheme_Object* obj)->Variant { return Variant(SCHEME_FLOAT_VAL(obj)); },
        [](Scheme_Object* obj)->Variant { return rktstr2gdstr(obj); },

        wrapper_to_variant<Scheme_Vector2>,
        wrapper_to_variant<Scheme_Vector2i>,
        wrapper_to_variant<Scheme_Rect2>,
        wrapper_to_variant<Scheme_Rect2i>,
        wrapper_to_variant<Scheme_Vector3>,
        wrapper_to_variant<Scheme_Vector3i>,
        wrapper_to_variant<Scheme_Transform2D>,
        wrapper_to_variant<Scheme_Vector4>,
        wrapper_to_variant<Scheme_Vector4i>,
        wrapper_to_variant<Scheme_Plane>,
        wrapper_to_variant<Scheme_Quaternion>,
        wrapper_to_variant<Scheme_AABB>,
        wrapper_to_variant<Scheme_Basis>,
        wrapper_to_variant<Scheme_Transform3D>,
        wrapper_to_variant<Scheme_Projection>,
        wrapper_to_variant<Scheme_Color>,

        [](Scheme_Object* obj)->Variant { return Variant(rktsym2gdstrname(obj)); },
        [](Scheme_Object* obj)->Variant { return Variant(NodePath(rktstr2gdstr(obj))); },
        wrapper_to_variant<Scheme_RID>,
        wrapper_to_variant<Scheme_GodotObject>,
        wrapper_to_variant<Scheme_Callable>,
        wrapper_to_variant<Scheme_Signal>,

        wrapper_to_variant<Scheme_Dictionary>,
        wrapper_to_variant<Scheme_Array>,

        wrapper_to_variant<Scheme_PackedByteArray>,
        wrapper_to_variant<Scheme_PackedInt32Array>,
        wrapper_to_variant<Scheme_PackedInt64Array>,
        wrapper_to_variant<Scheme_PackedFloat32Array>,
        wrapper_to_variant<Scheme_PackedFloat64Array>,
        wrapper_to_variant<Scheme_PackedStringArray>,
        wrapper_to_variant<Scheme_PackedVector2Array>,
        wrapper_to_variant<Scheme_PackedVector3Array>,
        wrapper_to_variant<Scheme_PackedColorArray>,
    };

    // Functions to convert from Variants to builtin wrappers.
    BuiltinToWrapper builtin_to_wrapper_funcs[Variant::Type::VARIANT_MAX] = {
        [](const Variant& v)->Scheme_Object* { return scheme_null; },
        [](const Variant& v)->Scheme_Object* { return (bool)v ? scheme_true : scheme_false; },
        [](const Variant& v)->Scheme_Object* { return scheme_make_integer((uint32_t)v); },
        [](const Variant& v)->Scheme_Object* { return scheme_make_float((float)v); },
        [](const Variant& v)->Scheme_Object* { return gdstr2rktstr(((String)v)); },

        variant_to_wrapper<Scheme_Vector2,     Vector2>,
        variant_to_wrapper<Scheme_Vector2i,    Vector2i>,
        variant_to_wrapper<Scheme_Rect2,       Rect2>,
        variant_to_wrapper<Scheme_Rect2i,      Rect2i>,
        variant_to_wrapper<Scheme_Vector3,     Vector3>,
        variant_to_wrapper<Scheme_Vector3i,    Vector3i>,
        variant_to_wrapper<Scheme_Transform2D, Transform2D>,
        variant_to_wrapper<Scheme_Vector4,     Vector4>,
        variant_to_wrapper<Scheme_Vector4i,    Vector4i>,
        variant_to_wrapper<Scheme_Plane,       Plane>,
        variant_to_wrapper<Scheme_Quaternion,  Quaternion>,
        variant_to_wrapper<Scheme_AABB,        AABB>,
        variant_to_wrapper<Scheme_Basis,       Basis>,
        variant_to_wrapper<Scheme_Transform3D, Transform3D>,
        variant_to_wrapper<Scheme_Projection,  Projection>,
        variant_to_wrapper<Scheme_Color,       Color>,

        [](const Variant& v)->Scheme_Object* { return gdstrname2rktsym(((StringName)v)); },
        [](const Variant& v)->Scheme_Object* { return gdstr2rktstr(((String)v)); },
        variant_to_wrapper<Scheme_RID, RID>,
        variant_to_wrapper<Scheme_GodotObject, Object*>,
        variant_to_wrapper<Scheme_Callable, Callable>,
        variant_to_wrapper<Scheme_Signal, Signal>,

        variant_to_wrapper<Scheme_Dictionary, Dictionary>,
        variant_to_wrapper<Scheme_Array, Array>,

        variant_to_wrapper<Scheme_PackedByteArray, PackedByteArray>,
        variant_to_wrapper<Scheme_PackedInt32Array, PackedInt32Array>,
        variant_to_wrapper<Scheme_PackedInt64Array, PackedInt64Array>,
        variant_to_wrapper<Scheme_PackedFloat32Array, PackedFloat32Array>,
        variant_to_wrapper<Scheme_PackedFloat64Array, PackedFloat64Array>,
        variant_to_wrapper<Scheme_PackedStringArray, PackedStringArray>,
        variant_to_wrapper<Scheme_PackedVector2Array, PackedVector2Array>,
        variant_to_wrapper<Scheme_PackedVector3Array, PackedVector3Array>,
        variant_to_wrapper<Scheme_PackedColorArray, PackedColorArray>
    };

public:
    static BuiltinBinder* get_singleton();

    // Allocates memory for a builtin wrapper with Racket.
    template <typename T>
    T* new_builtin_instance() {
        T* obj = (T*)scheme_malloc_allow_interior(sizeof(T));
        // TODO: haven't found if BC has a malloc method that sets the this type field automatically
        obj->header.so.type = T::scheme_type; //builtin_wrapper_type_id;
        obj->header.variant_type = T::wrapper_variant_type;
        return obj;
    }

    // Gets the Variant::Type associated with a given instance.
    Variant::Type get_variant_type(Scheme_Object* obj);

    // Allocates a Racket object and copies the value from the given Variant
    // into it.
    Scheme_Object* variant_to_scheme_object(const Variant& v);

    // Creates a Variant and copies the value from the given Racket object into
    // it.
    Variant scheme_object_to_variant(Scheme_Object* obj);

    // Returns true if the given Racket object wraps a Godot builtin, else
    // returns false.
    bool builtin_wrapperp(Scheme_Object* obj);

    // Registers builtin wrapper types with Racket. This must be called only
    // once while Racket is initializing before any instances are created.
    void register_builtin_types();
};


#endif // RACKET_BUILTIN_BINDER_H
