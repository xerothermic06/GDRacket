#include "./racket_builtin_binder.h"
#include "./racket_binder_util.h"

using namespace godot;

static Scheme_Object* Scheme_Vector2_ctor(int argc, Scheme_Object** argv) {
    RacketBCBuiltinBinder* binder = RacketBCBuiltinBinder::get_singleton();

    if (argc == 0) {
        Scheme_Vector2* obj = binder->new_builtin_instance<Scheme_Vector2>();
        obj->v = Vector2();
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::VECTOR2) {
        Scheme_Vector2* obj = binder->new_builtin_instance<Scheme_Vector2>();
        obj->v = Vector2(((Scheme_Vector2*)argv[0])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::VECTOR2I) {
        Scheme_Vector2* obj = binder->new_builtin_instance<Scheme_Vector2>();
        obj->v = Vector2(((Scheme_Vector2i*)argv[0])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 2 && SCHEME_FLOATP(argv[0]) && SCHEME_FLOATP(argv[1])) {
        Scheme_Vector2* obj = binder->new_builtin_instance<Scheme_Vector2>();
        obj->v = Vector2(SCHEME_FLOAT_VAL(argv[0]), SCHEME_FLOAT_VAL(argv[1]));
        return (Scheme_Object*)obj;
    }
    return nullptr;
}

static Scheme_Object* Scheme_Vector2i_ctor(int argc, Scheme_Object** argv) {
    RacketBCBuiltinBinder* binder = RacketBCBuiltinBinder::get_singleton();

    if (argc == 0) {
        Scheme_Vector2i* obj = binder->new_builtin_instance<Scheme_Vector2i>();
        obj->v = Vector2i();
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::VECTOR2I) {
        Scheme_Vector2i* obj = binder->new_builtin_instance<Scheme_Vector2i>();
        obj->v = Vector2i(((Scheme_Vector2i*)argv[0])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::VECTOR2) {
        Scheme_Vector2i* obj = binder->new_builtin_instance<Scheme_Vector2i>();
        obj->v = Vector2i(((Scheme_Vector2*)argv[0])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 2 && SCHEME_INTP(argv[0]) && SCHEME_INTP(argv[1])) {
        Scheme_Vector2i* obj = binder->new_builtin_instance<Scheme_Vector2i>();
        obj->v = Vector2i(SCHEME_INT_VAL(argv[0]), SCHEME_INT_VAL(argv[1]));
        return (Scheme_Object*)obj;
    }
    return nullptr;
}

static Scheme_Object* Scheme_Rect2_ctor(int argc, Scheme_Object** argv) {
    RacketBCBuiltinBinder* binder = RacketBCBuiltinBinder::get_singleton();

    if (argc == 0) {
        Scheme_Rect2* obj = binder->new_builtin_instance<Scheme_Rect2>();
        obj->v = Rect2();
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::RECT2) {
        Scheme_Rect2* obj = binder->new_builtin_instance<Scheme_Rect2>();
        obj->v = Rect2(((Scheme_Rect2*)argv[0])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::RECT2I) {
        Scheme_Rect2* obj = binder->new_builtin_instance<Scheme_Rect2>();
        obj->v = Rect2(((Scheme_Rect2i*)argv[0])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 2 && binder->get_variant_type(argv[0]) == Variant::Type::VECTOR2 && binder->get_variant_type(argv[1]) == Variant::Type::VECTOR2) {
        Scheme_Rect2* obj = binder->new_builtin_instance<Scheme_Rect2>();
        obj->v = Rect2(((Scheme_Vector2*)argv[0])->v, ((Scheme_Vector2*)argv[1])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 4 && SCHEME_FLOATP(argv[0]) && SCHEME_FLOATP(argv[1]) && SCHEME_FLOATP(argv[2]) && SCHEME_FLOATP(argv[3])) {
        Scheme_Rect2* obj = binder->new_builtin_instance<Scheme_Rect2>();
        obj->v = Rect2(SCHEME_FLOAT_VAL(argv[0]), SCHEME_FLOAT_VAL(argv[1]), SCHEME_FLOAT_VAL(argv[2]), SCHEME_FLOAT_VAL(argv[3]));
        return (Scheme_Object*)obj;
    }
    return nullptr;
}

static Scheme_Object* Scheme_Rect2i_ctor(int argc, Scheme_Object** argv) {
    RacketBCBuiltinBinder* binder = RacketBCBuiltinBinder::get_singleton();

    if (argc == 0) {
        Scheme_Rect2i* obj = binder->new_builtin_instance<Scheme_Rect2i>();
        obj->v = Rect2i();
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::RECT2I) {
        Scheme_Rect2i* obj = binder->new_builtin_instance<Scheme_Rect2i>();
        obj->v = Rect2i(((Scheme_Rect2i*)argv[0])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::RECT2) {
        Scheme_Rect2i* obj = binder->new_builtin_instance<Scheme_Rect2i>();
        obj->v = Rect2i(((Scheme_Rect2*)argv[0])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 2 && binder->get_variant_type(argv[0]) == Variant::Type::VECTOR2I && binder->get_variant_type(argv[1]) == Variant::Type::VECTOR2I) {
        Scheme_Rect2i* obj = binder->new_builtin_instance<Scheme_Rect2i>();
        obj->v = Rect2i(((Scheme_Vector2i*)argv[0])->v, ((Scheme_Vector2i*)argv[1])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 4 && SCHEME_INTP(argv[0]) && SCHEME_INTP(argv[1]) && SCHEME_INTP(argv[2]) && SCHEME_INTP(argv[3])) {
        Scheme_Rect2i* obj = binder->new_builtin_instance<Scheme_Rect2i>();
        obj->v = Rect2i(SCHEME_INT_VAL(argv[0]), SCHEME_INT_VAL(argv[1]), SCHEME_INT_VAL(argv[2]), SCHEME_INT_VAL(argv[3]));
        return (Scheme_Object*)obj;
    }
    return nullptr;
}

static Scheme_Object* Scheme_Vector3_ctor(int argc, Scheme_Object** argv) {
    RacketBCBuiltinBinder* binder = RacketBCBuiltinBinder::get_singleton();

    if (argc == 0) {
        Scheme_Vector3* obj = binder->new_builtin_instance<Scheme_Vector3>();
        obj->v = Vector3();
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::VECTOR3) {
        Scheme_Vector3* obj = binder->new_builtin_instance<Scheme_Vector3>();
        obj->v = Vector3(((Scheme_Vector3*)argv[0])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::VECTOR3I) {
        Scheme_Vector3* obj = binder->new_builtin_instance<Scheme_Vector3>();
        obj->v = Vector3(((Scheme_Vector3i*)argv[0])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 3 && SCHEME_FLOATP(argv[0]) && SCHEME_FLOATP(argv[1]) && SCHEME_FLOATP(argv[2])) {
        Scheme_Vector3* obj = binder->new_builtin_instance<Scheme_Vector3>();
        obj->v = Vector3(SCHEME_FLOAT_VAL(argv[0]), SCHEME_FLOAT_VAL(argv[1]), SCHEME_FLOAT_VAL(argv[2]));
        return (Scheme_Object*)obj;
    }
    return nullptr;
}

static Scheme_Object* Scheme_Vector3i_ctor(int argc, Scheme_Object** argv) {
    RacketBCBuiltinBinder* binder = RacketBCBuiltinBinder::get_singleton();

    if (argc == 0) {
        Scheme_Vector3i* obj = binder->new_builtin_instance<Scheme_Vector3i>();
        obj->v = Vector3i();
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::VECTOR3I) {
        Scheme_Vector3i* obj = binder->new_builtin_instance<Scheme_Vector3i>();
        obj->v = Vector3i(((Scheme_Vector3i*)argv[0])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::VECTOR3) {
        Scheme_Vector3i* obj = binder->new_builtin_instance<Scheme_Vector3i>();
        obj->v = Vector3i(((Scheme_Vector3*)argv[0])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 3 && SCHEME_INTP(argv[0]) && SCHEME_INTP(argv[1]) && SCHEME_INTP(argv[2])) {
        Scheme_Vector3i* obj = binder->new_builtin_instance<Scheme_Vector3i>();
        obj->v = Vector3i(SCHEME_INT_VAL(argv[0]), SCHEME_INT_VAL(argv[1]), SCHEME_INT_VAL(argv[2]));
        return (Scheme_Object*)obj;
    }
    return nullptr;
}

static Scheme_Object* Scheme_Transform2D_ctor(int argc, Scheme_Object** argv) {
    RacketBCBuiltinBinder* binder = RacketBCBuiltinBinder::get_singleton();

    if (argc == 0) {
        Scheme_Transform2D* obj = binder->new_builtin_instance<Scheme_Transform2D>();
        obj->v = Transform2D();
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::TRANSFORM2D) {
        Scheme_Transform2D* obj = binder->new_builtin_instance<Scheme_Transform2D>();
        obj->v = Transform2D(((Scheme_Transform2D*)argv[0])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 2 && SCHEME_FLOATP(argv[0]) && binder->get_variant_type(argv[1]) == Variant::Type::VECTOR2) {
        Scheme_Transform2D* obj = binder->new_builtin_instance<Scheme_Transform2D>();
        obj->v = Transform2D(SCHEME_FLOAT_VAL(argv[0]), ((Scheme_Vector2*)argv[1])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 4 && SCHEME_FLOATP(argv[0]) && binder->get_variant_type(argv[1]) == Variant::Type::VECTOR2 && SCHEME_FLOATP(argv[2]) && binder->get_variant_type(argv[3]) == Variant::Type::VECTOR2) {
        Scheme_Transform2D* obj = binder->new_builtin_instance<Scheme_Transform2D>();
        obj->v = Transform2D(SCHEME_FLOAT_VAL(argv[0]), ((Scheme_Vector2*)argv[1])->v, SCHEME_FLOAT_VAL(argv[2]), ((Scheme_Vector2*)argv[3])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 3 && binder->get_variant_type(argv[0]) == Variant::Type::VECTOR2 && binder->get_variant_type(argv[1]) == Variant::Type::VECTOR2 && binder->get_variant_type(argv[2]) == Variant::Type::VECTOR2) {
        Scheme_Transform2D* obj = binder->new_builtin_instance<Scheme_Transform2D>();
        obj->v = Transform2D(((Scheme_Vector2*)argv[0])->v, ((Scheme_Vector2*)argv[1])->v, ((Scheme_Vector2*)argv[2])->v);
        return (Scheme_Object*)obj;
    }
    return nullptr;
}

static Scheme_Object* Scheme_Vector4_ctor(int argc, Scheme_Object** argv) {
    RacketBCBuiltinBinder* binder = RacketBCBuiltinBinder::get_singleton();

    if (argc == 0) {
        Scheme_Vector4* obj = binder->new_builtin_instance<Scheme_Vector4>();
        obj->v = Vector4();
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::VECTOR4) {
        Scheme_Vector4* obj = binder->new_builtin_instance<Scheme_Vector4>();
        obj->v = Vector4(((Scheme_Vector4*)argv[0])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::VECTOR4I) {
        Scheme_Vector4* obj = binder->new_builtin_instance<Scheme_Vector4>();
        obj->v = Vector4(((Scheme_Vector4i*)argv[0])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 4 && SCHEME_FLOATP(argv[0]) && SCHEME_FLOATP(argv[1]) && SCHEME_FLOATP(argv[2]) && SCHEME_FLOATP(argv[3])) {
        Scheme_Vector4* obj = binder->new_builtin_instance<Scheme_Vector4>();
        obj->v = Vector4(SCHEME_FLOAT_VAL(argv[0]), SCHEME_FLOAT_VAL(argv[1]), SCHEME_FLOAT_VAL(argv[2]), SCHEME_FLOAT_VAL(argv[3]));
        return (Scheme_Object*)obj;
    }
    return nullptr;
}

static Scheme_Object* Scheme_Vector4i_ctor(int argc, Scheme_Object** argv) {
    RacketBCBuiltinBinder* binder = RacketBCBuiltinBinder::get_singleton();

    if (argc == 0) {
        Scheme_Vector4i* obj = binder->new_builtin_instance<Scheme_Vector4i>();
        obj->v = Vector4i();
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::VECTOR4I) {
        Scheme_Vector4i* obj = binder->new_builtin_instance<Scheme_Vector4i>();
        obj->v = Vector4i(((Scheme_Vector4i*)argv[0])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::VECTOR4) {
        Scheme_Vector4i* obj = binder->new_builtin_instance<Scheme_Vector4i>();
        obj->v = Vector4i(((Scheme_Vector4*)argv[0])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 4 && SCHEME_INTP(argv[0]) && SCHEME_INTP(argv[1]) && SCHEME_INTP(argv[2]) && SCHEME_INTP(argv[3])) {
        Scheme_Vector4i* obj = binder->new_builtin_instance<Scheme_Vector4i>();
        obj->v = Vector4i(SCHEME_INT_VAL(argv[0]), SCHEME_INT_VAL(argv[1]), SCHEME_INT_VAL(argv[2]), SCHEME_INT_VAL(argv[3]));
        return (Scheme_Object*)obj;
    }
    return nullptr;
}

static Scheme_Object* Scheme_Plane_ctor(int argc, Scheme_Object** argv) {
    RacketBCBuiltinBinder* binder = RacketBCBuiltinBinder::get_singleton();

    if (argc == 0) {
        Scheme_Plane* obj = binder->new_builtin_instance<Scheme_Plane>();
        obj->v = Plane();
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::PLANE) {
        Scheme_Plane* obj = binder->new_builtin_instance<Scheme_Plane>();
        obj->v = Plane(((Scheme_Plane*)argv[0])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::VECTOR3) {
        Scheme_Plane* obj = binder->new_builtin_instance<Scheme_Plane>();
        obj->v = Plane(((Scheme_Vector3*)argv[0])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 2 && binder->get_variant_type(argv[0]) == Variant::Type::VECTOR3 && SCHEME_FLOATP(argv[1])) {
        Scheme_Plane* obj = binder->new_builtin_instance<Scheme_Plane>();
        obj->v = Plane(((Scheme_Vector3*)argv[0])->v, SCHEME_FLOAT_VAL(argv[1]));
        return (Scheme_Object*)obj;
    }

    else if (argc == 2 && binder->get_variant_type(argv[0]) == Variant::Type::VECTOR3 && binder->get_variant_type(argv[1]) == Variant::Type::VECTOR3) {
        Scheme_Plane* obj = binder->new_builtin_instance<Scheme_Plane>();
        obj->v = Plane(((Scheme_Vector3*)argv[0])->v, ((Scheme_Vector3*)argv[1])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 3 && binder->get_variant_type(argv[0]) == Variant::Type::VECTOR3 && binder->get_variant_type(argv[1]) == Variant::Type::VECTOR3 && binder->get_variant_type(argv[2]) == Variant::Type::VECTOR3) {
        Scheme_Plane* obj = binder->new_builtin_instance<Scheme_Plane>();
        obj->v = Plane(((Scheme_Vector3*)argv[0])->v, ((Scheme_Vector3*)argv[1])->v, ((Scheme_Vector3*)argv[2])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 4 && SCHEME_FLOATP(argv[0]) && SCHEME_FLOATP(argv[1]) && SCHEME_FLOATP(argv[2]) && SCHEME_FLOATP(argv[3])) {
        Scheme_Plane* obj = binder->new_builtin_instance<Scheme_Plane>();
        obj->v = Plane(SCHEME_FLOAT_VAL(argv[0]), SCHEME_FLOAT_VAL(argv[1]), SCHEME_FLOAT_VAL(argv[2]), SCHEME_FLOAT_VAL(argv[3]));
        return (Scheme_Object*)obj;
    }
    return nullptr;
}

static Scheme_Object* Scheme_Quaternion_ctor(int argc, Scheme_Object** argv) {
    RacketBCBuiltinBinder* binder = RacketBCBuiltinBinder::get_singleton();

    if (argc == 0) {
        Scheme_Quaternion* obj = binder->new_builtin_instance<Scheme_Quaternion>();
        obj->v = Quaternion();
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::QUATERNION) {
        Scheme_Quaternion* obj = binder->new_builtin_instance<Scheme_Quaternion>();
        obj->v = Quaternion(((Scheme_Quaternion*)argv[0])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::BASIS) {
        Scheme_Quaternion* obj = binder->new_builtin_instance<Scheme_Quaternion>();
        obj->v = Quaternion(((Scheme_Basis*)argv[0])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 2 && binder->get_variant_type(argv[0]) == Variant::Type::VECTOR3 && SCHEME_FLOATP(argv[1])) {
        Scheme_Quaternion* obj = binder->new_builtin_instance<Scheme_Quaternion>();
        obj->v = Quaternion(((Scheme_Vector3*)argv[0])->v, SCHEME_FLOAT_VAL(argv[1]));
        return (Scheme_Object*)obj;
    }

    else if (argc == 2 && binder->get_variant_type(argv[0]) == Variant::Type::VECTOR3 && binder->get_variant_type(argv[1]) == Variant::Type::VECTOR3) {
        Scheme_Quaternion* obj = binder->new_builtin_instance<Scheme_Quaternion>();
        obj->v = Quaternion(((Scheme_Vector3*)argv[0])->v, ((Scheme_Vector3*)argv[1])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 4 && SCHEME_FLOATP(argv[0]) && SCHEME_FLOATP(argv[1]) && SCHEME_FLOATP(argv[2]) && SCHEME_FLOATP(argv[3])) {
        Scheme_Quaternion* obj = binder->new_builtin_instance<Scheme_Quaternion>();
        obj->v = Quaternion(SCHEME_FLOAT_VAL(argv[0]), SCHEME_FLOAT_VAL(argv[1]), SCHEME_FLOAT_VAL(argv[2]), SCHEME_FLOAT_VAL(argv[3]));
        return (Scheme_Object*)obj;
    }
    return nullptr;
}

static Scheme_Object* Scheme_AABB_ctor(int argc, Scheme_Object** argv) {
    RacketBCBuiltinBinder* binder = RacketBCBuiltinBinder::get_singleton();

    if (argc == 0) {
        Scheme_AABB* obj = binder->new_builtin_instance<Scheme_AABB>();
        obj->v = AABB();
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::AABB) {
        Scheme_AABB* obj = binder->new_builtin_instance<Scheme_AABB>();
        obj->v = AABB(((Scheme_AABB*)argv[0])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 2 && binder->get_variant_type(argv[0]) == Variant::Type::VECTOR3 && binder->get_variant_type(argv[1]) == Variant::Type::VECTOR3) {
        Scheme_AABB* obj = binder->new_builtin_instance<Scheme_AABB>();
        obj->v = AABB(((Scheme_Vector3*)argv[0])->v, ((Scheme_Vector3*)argv[1])->v);
        return (Scheme_Object*)obj;
    }
    return nullptr;
}

static Scheme_Object* Scheme_Basis_ctor(int argc, Scheme_Object** argv) {
    RacketBCBuiltinBinder* binder = RacketBCBuiltinBinder::get_singleton();

    if (argc == 0) {
        Scheme_Basis* obj = binder->new_builtin_instance<Scheme_Basis>();
        obj->v = Basis();
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::BASIS) {
        Scheme_Basis* obj = binder->new_builtin_instance<Scheme_Basis>();
        obj->v = Basis(((Scheme_Basis*)argv[0])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::QUATERNION) {
        Scheme_Basis* obj = binder->new_builtin_instance<Scheme_Basis>();
        obj->v = Basis(((Scheme_Quaternion*)argv[0])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 2 && binder->get_variant_type(argv[0]) == Variant::Type::VECTOR3 && SCHEME_FLOATP(argv[1])) {
        Scheme_Basis* obj = binder->new_builtin_instance<Scheme_Basis>();
        obj->v = Basis(((Scheme_Vector3*)argv[0])->v, SCHEME_FLOAT_VAL(argv[1]));
        return (Scheme_Object*)obj;
    }

    else if (argc == 3 && binder->get_variant_type(argv[0]) == Variant::Type::VECTOR3 && binder->get_variant_type(argv[1]) == Variant::Type::VECTOR3 && binder->get_variant_type(argv[2]) == Variant::Type::VECTOR3) {
        Scheme_Basis* obj = binder->new_builtin_instance<Scheme_Basis>();
        obj->v = Basis(((Scheme_Vector3*)argv[0])->v, ((Scheme_Vector3*)argv[1])->v, ((Scheme_Vector3*)argv[2])->v);
        return (Scheme_Object*)obj;
    }
    return nullptr;
}

static Scheme_Object* Scheme_Transform3D_ctor(int argc, Scheme_Object** argv) {
    RacketBCBuiltinBinder* binder = RacketBCBuiltinBinder::get_singleton();

    if (argc == 0) {
        Scheme_Transform3D* obj = binder->new_builtin_instance<Scheme_Transform3D>();
        obj->v = Transform3D();
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::TRANSFORM3D) {
        Scheme_Transform3D* obj = binder->new_builtin_instance<Scheme_Transform3D>();
        obj->v = Transform3D(((Scheme_Transform3D*)argv[0])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 2 && binder->get_variant_type(argv[0]) == Variant::Type::BASIS && binder->get_variant_type(argv[1]) == Variant::Type::VECTOR3) {
        Scheme_Transform3D* obj = binder->new_builtin_instance<Scheme_Transform3D>();
        obj->v = Transform3D(((Scheme_Basis*)argv[0])->v, ((Scheme_Vector3*)argv[1])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 4 && binder->get_variant_type(argv[0]) == Variant::Type::VECTOR3 && binder->get_variant_type(argv[1]) == Variant::Type::VECTOR3 && binder->get_variant_type(argv[2]) == Variant::Type::VECTOR3 && binder->get_variant_type(argv[3]) == Variant::Type::VECTOR3) {
        Scheme_Transform3D* obj = binder->new_builtin_instance<Scheme_Transform3D>();
        obj->v = Transform3D(((Scheme_Vector3*)argv[0])->v, ((Scheme_Vector3*)argv[1])->v, ((Scheme_Vector3*)argv[2])->v, ((Scheme_Vector3*)argv[3])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::PROJECTION) {
        Scheme_Transform3D* obj = binder->new_builtin_instance<Scheme_Transform3D>();
        obj->v = Transform3D(((Scheme_Projection*)argv[0])->v);
        return (Scheme_Object*)obj;
    }
    return nullptr;
}

static Scheme_Object* Scheme_Projection_ctor(int argc, Scheme_Object** argv) {
    RacketBCBuiltinBinder* binder = RacketBCBuiltinBinder::get_singleton();

    if (argc == 0) {
        Scheme_Projection* obj = binder->new_builtin_instance<Scheme_Projection>();
        obj->v = Projection();
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::PROJECTION) {
        Scheme_Projection* obj = binder->new_builtin_instance<Scheme_Projection>();
        obj->v = Projection(((Scheme_Projection*)argv[0])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::TRANSFORM3D) {
        Scheme_Projection* obj = binder->new_builtin_instance<Scheme_Projection>();
        obj->v = Projection(((Scheme_Transform3D*)argv[0])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 4 && binder->get_variant_type(argv[0]) == Variant::Type::VECTOR4 && binder->get_variant_type(argv[1]) == Variant::Type::VECTOR4 && binder->get_variant_type(argv[2]) == Variant::Type::VECTOR4 && binder->get_variant_type(argv[3]) == Variant::Type::VECTOR4) {
        Scheme_Projection* obj = binder->new_builtin_instance<Scheme_Projection>();
        obj->v = Projection(((Scheme_Vector4*)argv[0])->v, ((Scheme_Vector4*)argv[1])->v, ((Scheme_Vector4*)argv[2])->v, ((Scheme_Vector4*)argv[3])->v);
        return (Scheme_Object*)obj;
    }
    return nullptr;
}

static Scheme_Object* Scheme_Color_ctor(int argc, Scheme_Object** argv) {
    RacketBCBuiltinBinder* binder = RacketBCBuiltinBinder::get_singleton();

    if (argc == 0) {
        Scheme_Color* obj = binder->new_builtin_instance<Scheme_Color>();
        obj->v = Color();
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::COLOR) {
        Scheme_Color* obj = binder->new_builtin_instance<Scheme_Color>();
        obj->v = Color(((Scheme_Color*)argv[0])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 2 && binder->get_variant_type(argv[0]) == Variant::Type::COLOR && SCHEME_FLOATP(argv[1])) {
        Scheme_Color* obj = binder->new_builtin_instance<Scheme_Color>();
        obj->v = Color(((Scheme_Color*)argv[0])->v, SCHEME_FLOAT_VAL(argv[1]));
        return (Scheme_Object*)obj;
    }

    else if (argc == 3 && SCHEME_FLOATP(argv[0]) && SCHEME_FLOATP(argv[1]) && SCHEME_FLOATP(argv[2])) {
        Scheme_Color* obj = binder->new_builtin_instance<Scheme_Color>();
        obj->v = Color(SCHEME_FLOAT_VAL(argv[0]), SCHEME_FLOAT_VAL(argv[1]), SCHEME_FLOAT_VAL(argv[2]));
        return (Scheme_Object*)obj;
    }

    else if (argc == 4 && SCHEME_FLOATP(argv[0]) && SCHEME_FLOATP(argv[1]) && SCHEME_FLOATP(argv[2]) && SCHEME_FLOATP(argv[3])) {
        Scheme_Color* obj = binder->new_builtin_instance<Scheme_Color>();
        obj->v = Color(SCHEME_FLOAT_VAL(argv[0]), SCHEME_FLOAT_VAL(argv[1]), SCHEME_FLOAT_VAL(argv[2]), SCHEME_FLOAT_VAL(argv[3]));
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && SCHEME_CHAR_STRINGP(argv[0])) {
        Scheme_Color* obj = binder->new_builtin_instance<Scheme_Color>();
        obj->v = Color(rktstr2gdstr(argv[0]));
        return (Scheme_Object*)obj;
    }

    else if (argc == 2 && SCHEME_CHAR_STRINGP(argv[0]) && SCHEME_FLOATP(argv[1])) {
        Scheme_Color* obj = binder->new_builtin_instance<Scheme_Color>();
        obj->v = Color(rktstr2gdstr(argv[0]), SCHEME_FLOAT_VAL(argv[1]));
        return (Scheme_Object*)obj;
    }
    return nullptr;
}

static Scheme_Object* Scheme_RID_ctor(int argc, Scheme_Object** argv) {
    RacketBCBuiltinBinder* binder = RacketBCBuiltinBinder::get_singleton();

    if (argc == 0) {
        Scheme_RID* obj = binder->new_builtin_instance<Scheme_RID>();
        obj->v = RID();
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::RID) {
        Scheme_RID* obj = binder->new_builtin_instance<Scheme_RID>();
        obj->v = RID(((Scheme_RID*)argv[0])->v);
        return (Scheme_Object*)obj;
    }
    return nullptr;
}

static Scheme_Object* Scheme_Callable_ctor(int argc, Scheme_Object** argv) {
    RacketBCBuiltinBinder* binder = RacketBCBuiltinBinder::get_singleton();

    if (argc == 0) {
        Scheme_Callable* obj = binder->new_builtin_instance<Scheme_Callable>();
        obj->v = Callable();
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::CALLABLE) {
        Scheme_Callable* obj = binder->new_builtin_instance<Scheme_Callable>();
        obj->v = Callable(((Scheme_Callable*)argv[0])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 2 && binder->get_variant_type(argv[0]) == Variant::Type::OBJECT && binder->get_variant_type(argv[1]) == Variant::Type::STRING_NAME) {
        Scheme_Callable* obj = binder->new_builtin_instance<Scheme_Callable>();
        obj->v = Callable(((Scheme_GodotObject*)argv[0])->v, ((Scheme_StringName*)argv[1])->v);
        return (Scheme_Object*)obj;
    }
    return nullptr;
}

static Scheme_Object* Scheme_Signal_ctor(int argc, Scheme_Object** argv) {
    RacketBCBuiltinBinder* binder = RacketBCBuiltinBinder::get_singleton();

    if (argc == 0) {
        Scheme_Signal* obj = binder->new_builtin_instance<Scheme_Signal>();
        obj->v = Signal();
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::SIGNAL) {
        Scheme_Signal* obj = binder->new_builtin_instance<Scheme_Signal>();
        obj->v = Signal(((Scheme_Signal*)argv[0])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 2 && binder->get_variant_type(argv[0]) == Variant::Type::OBJECT && binder->get_variant_type(argv[1]) == Variant::Type::STRING_NAME) {
        Scheme_Signal* obj = binder->new_builtin_instance<Scheme_Signal>();
        obj->v = Signal(((Scheme_GodotObject*)argv[0])->v, ((Scheme_StringName*)argv[1])->v);
        return (Scheme_Object*)obj;
    }
    return nullptr;
}

static Scheme_Object* Scheme_Dictionary_ctor(int argc, Scheme_Object** argv) {
    RacketBCBuiltinBinder* binder = RacketBCBuiltinBinder::get_singleton();

    if (argc == 0) {
        Scheme_Dictionary* obj = binder->new_builtin_instance<Scheme_Dictionary>();
        obj->v = Dictionary();
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::DICTIONARY) {
        Scheme_Dictionary* obj = binder->new_builtin_instance<Scheme_Dictionary>();
        obj->v = Dictionary(((Scheme_Dictionary*)argv[0])->v);
        return (Scheme_Object*)obj;
    }
    return nullptr;
}

static Scheme_Object* Scheme_Array_ctor(int argc, Scheme_Object** argv) {
    RacketBCBuiltinBinder* binder = RacketBCBuiltinBinder::get_singleton();

    if (argc == 0) {
        Scheme_Array* obj = binder->new_builtin_instance<Scheme_Array>();
        obj->v = Array();
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::ARRAY) {
        Scheme_Array* obj = binder->new_builtin_instance<Scheme_Array>();
        obj->v = Array(((Scheme_Array*)argv[0])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 4 && binder->get_variant_type(argv[0]) == Variant::Type::ARRAY && SCHEME_INTP(argv[1]) && binder->get_variant_type(argv[2]) == Variant::Type::STRING_NAME && binder->builtin_wrapperp(argv[3])) {
        Scheme_Array* obj = binder->new_builtin_instance<Scheme_Array>();
        obj->v = Array(((Scheme_Array*)argv[0])->v, SCHEME_INT_VAL(argv[1]), ((Scheme_StringName*)argv[2])->v, binder->scheme_object_to_variant(argv[3]));
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::PACKED_BYTE_ARRAY) {
        Scheme_Array* obj = binder->new_builtin_instance<Scheme_Array>();
        obj->v = Array(((Scheme_PackedByteArray*)argv[0])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::PACKED_INT32_ARRAY) {
        Scheme_Array* obj = binder->new_builtin_instance<Scheme_Array>();
        obj->v = Array(((Scheme_PackedInt32Array*)argv[0])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::PACKED_INT64_ARRAY) {
        Scheme_Array* obj = binder->new_builtin_instance<Scheme_Array>();
        obj->v = Array(((Scheme_PackedInt64Array*)argv[0])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::PACKED_FLOAT32_ARRAY) {
        Scheme_Array* obj = binder->new_builtin_instance<Scheme_Array>();
        obj->v = Array(((Scheme_PackedFloat32Array*)argv[0])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::PACKED_FLOAT64_ARRAY) {
        Scheme_Array* obj = binder->new_builtin_instance<Scheme_Array>();
        obj->v = Array(((Scheme_PackedFloat64Array*)argv[0])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::PACKED_STRING_ARRAY) {
        Scheme_Array* obj = binder->new_builtin_instance<Scheme_Array>();
        obj->v = Array(((Scheme_PackedStringArray*)argv[0])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::PACKED_VECTOR2_ARRAY) {
        Scheme_Array* obj = binder->new_builtin_instance<Scheme_Array>();
        obj->v = Array(((Scheme_PackedVector2Array*)argv[0])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::PACKED_VECTOR3_ARRAY) {
        Scheme_Array* obj = binder->new_builtin_instance<Scheme_Array>();
        obj->v = Array(((Scheme_PackedVector3Array*)argv[0])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::PACKED_COLOR_ARRAY) {
        Scheme_Array* obj = binder->new_builtin_instance<Scheme_Array>();
        obj->v = Array(((Scheme_PackedColorArray*)argv[0])->v);
        return (Scheme_Object*)obj;
    }
    return nullptr;
}

static Scheme_Object* Scheme_PackedByteArray_ctor(int argc, Scheme_Object** argv) {
    RacketBCBuiltinBinder* binder = RacketBCBuiltinBinder::get_singleton();

    if (argc == 0) {
        Scheme_PackedByteArray* obj = binder->new_builtin_instance<Scheme_PackedByteArray>();
        obj->v = PackedByteArray();
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::PACKED_BYTE_ARRAY) {
        Scheme_PackedByteArray* obj = binder->new_builtin_instance<Scheme_PackedByteArray>();
        obj->v = PackedByteArray(((Scheme_PackedByteArray*)argv[0])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::ARRAY) {
        Scheme_PackedByteArray* obj = binder->new_builtin_instance<Scheme_PackedByteArray>();
        obj->v = PackedByteArray(((Scheme_Array*)argv[0])->v);
        return (Scheme_Object*)obj;
    }
    return nullptr;
}

static Scheme_Object* Scheme_PackedInt32Array_ctor(int argc, Scheme_Object** argv) {
    RacketBCBuiltinBinder* binder = RacketBCBuiltinBinder::get_singleton();

    if (argc == 0) {
        Scheme_PackedInt32Array* obj = binder->new_builtin_instance<Scheme_PackedInt32Array>();
        obj->v = PackedInt32Array();
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::PACKED_INT32_ARRAY) {
        Scheme_PackedInt32Array* obj = binder->new_builtin_instance<Scheme_PackedInt32Array>();
        obj->v = PackedInt32Array(((Scheme_PackedInt32Array*)argv[0])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::ARRAY) {
        Scheme_PackedInt32Array* obj = binder->new_builtin_instance<Scheme_PackedInt32Array>();
        obj->v = PackedInt32Array(((Scheme_Array*)argv[0])->v);
        return (Scheme_Object*)obj;
    }
    return nullptr;
}

static Scheme_Object* Scheme_PackedInt64Array_ctor(int argc, Scheme_Object** argv) {
    RacketBCBuiltinBinder* binder = RacketBCBuiltinBinder::get_singleton();

    if (argc == 0) {
        Scheme_PackedInt64Array* obj = binder->new_builtin_instance<Scheme_PackedInt64Array>();
        obj->v = PackedInt64Array();
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::PACKED_INT64_ARRAY) {
        Scheme_PackedInt64Array* obj = binder->new_builtin_instance<Scheme_PackedInt64Array>();
        obj->v = PackedInt64Array(((Scheme_PackedInt64Array*)argv[0])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::ARRAY) {
        Scheme_PackedInt64Array* obj = binder->new_builtin_instance<Scheme_PackedInt64Array>();
        obj->v = PackedInt64Array(((Scheme_Array*)argv[0])->v);
        return (Scheme_Object*)obj;
    }
    return nullptr;
}

static Scheme_Object* Scheme_PackedFloat32Array_ctor(int argc, Scheme_Object** argv) {
    RacketBCBuiltinBinder* binder = RacketBCBuiltinBinder::get_singleton();

    if (argc == 0) {
        Scheme_PackedFloat32Array* obj = binder->new_builtin_instance<Scheme_PackedFloat32Array>();
        obj->v = PackedFloat32Array();
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::PACKED_FLOAT32_ARRAY) {
        Scheme_PackedFloat32Array* obj = binder->new_builtin_instance<Scheme_PackedFloat32Array>();
        obj->v = PackedFloat32Array(((Scheme_PackedFloat32Array*)argv[0])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::ARRAY) {
        Scheme_PackedFloat32Array* obj = binder->new_builtin_instance<Scheme_PackedFloat32Array>();
        obj->v = PackedFloat32Array(((Scheme_Array*)argv[0])->v);
        return (Scheme_Object*)obj;
    }
    return nullptr;
}

static Scheme_Object* Scheme_PackedFloat64Array_ctor(int argc, Scheme_Object** argv) {
    RacketBCBuiltinBinder* binder = RacketBCBuiltinBinder::get_singleton();

    if (argc == 0) {
        Scheme_PackedFloat64Array* obj = binder->new_builtin_instance<Scheme_PackedFloat64Array>();
        obj->v = PackedFloat64Array();
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::PACKED_FLOAT64_ARRAY) {
        Scheme_PackedFloat64Array* obj = binder->new_builtin_instance<Scheme_PackedFloat64Array>();
        obj->v = PackedFloat64Array(((Scheme_PackedFloat64Array*)argv[0])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::ARRAY) {
        Scheme_PackedFloat64Array* obj = binder->new_builtin_instance<Scheme_PackedFloat64Array>();
        obj->v = PackedFloat64Array(((Scheme_Array*)argv[0])->v);
        return (Scheme_Object*)obj;
    }
    return nullptr;
}

static Scheme_Object* Scheme_PackedStringArray_ctor(int argc, Scheme_Object** argv) {
    RacketBCBuiltinBinder* binder = RacketBCBuiltinBinder::get_singleton();

    if (argc == 0) {
        Scheme_PackedStringArray* obj = binder->new_builtin_instance<Scheme_PackedStringArray>();
        obj->v = PackedStringArray();
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::PACKED_STRING_ARRAY) {
        Scheme_PackedStringArray* obj = binder->new_builtin_instance<Scheme_PackedStringArray>();
        obj->v = PackedStringArray(((Scheme_PackedStringArray*)argv[0])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::ARRAY) {
        Scheme_PackedStringArray* obj = binder->new_builtin_instance<Scheme_PackedStringArray>();
        obj->v = PackedStringArray(((Scheme_Array*)argv[0])->v);
        return (Scheme_Object*)obj;
    }
    return nullptr;
}

static Scheme_Object* Scheme_PackedVector2Array_ctor(int argc, Scheme_Object** argv) {
    RacketBCBuiltinBinder* binder = RacketBCBuiltinBinder::get_singleton();

    if (argc == 0) {
        Scheme_PackedVector2Array* obj = binder->new_builtin_instance<Scheme_PackedVector2Array>();
        obj->v = PackedVector2Array();
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::PACKED_VECTOR2_ARRAY) {
        Scheme_PackedVector2Array* obj = binder->new_builtin_instance<Scheme_PackedVector2Array>();
        obj->v = PackedVector2Array(((Scheme_PackedVector2Array*)argv[0])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::ARRAY) {
        Scheme_PackedVector2Array* obj = binder->new_builtin_instance<Scheme_PackedVector2Array>();
        obj->v = PackedVector2Array(((Scheme_Array*)argv[0])->v);
        return (Scheme_Object*)obj;
    }
    return nullptr;
}

static Scheme_Object* Scheme_PackedVector3Array_ctor(int argc, Scheme_Object** argv) {
    RacketBCBuiltinBinder* binder = RacketBCBuiltinBinder::get_singleton();

    if (argc == 0) {
        Scheme_PackedVector3Array* obj = binder->new_builtin_instance<Scheme_PackedVector3Array>();
        obj->v = PackedVector3Array();
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::PACKED_VECTOR3_ARRAY) {
        Scheme_PackedVector3Array* obj = binder->new_builtin_instance<Scheme_PackedVector3Array>();
        obj->v = PackedVector3Array(((Scheme_PackedVector3Array*)argv[0])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::ARRAY) {
        Scheme_PackedVector3Array* obj = binder->new_builtin_instance<Scheme_PackedVector3Array>();
        obj->v = PackedVector3Array(((Scheme_Array*)argv[0])->v);
        return (Scheme_Object*)obj;
    }
    return nullptr;
}

static Scheme_Object* Scheme_PackedColorArray_ctor(int argc, Scheme_Object** argv) {
    RacketBCBuiltinBinder* binder = RacketBCBuiltinBinder::get_singleton();

    if (argc == 0) {
        Scheme_PackedColorArray* obj = binder->new_builtin_instance<Scheme_PackedColorArray>();
        obj->v = PackedColorArray();
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::PACKED_COLOR_ARRAY) {
        Scheme_PackedColorArray* obj = binder->new_builtin_instance<Scheme_PackedColorArray>();
        obj->v = PackedColorArray(((Scheme_PackedColorArray*)argv[0])->v);
        return (Scheme_Object*)obj;
    }

    else if (argc == 1 && binder->get_variant_type(argv[0]) == Variant::Type::ARRAY) {
        Scheme_PackedColorArray* obj = binder->new_builtin_instance<Scheme_PackedColorArray>();
        obj->v = PackedColorArray(((Scheme_Array*)argv[0])->v);
        return (Scheme_Object*)obj;
    }
    return nullptr;
}
void register_builtin_wrapper_ctors(Scheme_Env* module) {
    scheme_add_global("Vector2", scheme_make_prim_w_everything(Scheme_Vector2_ctor, 1, "Vector2", 0, 2, 0, 1, 1), module);
    scheme_add_global("Vector2i", scheme_make_prim_w_everything(Scheme_Vector2i_ctor, 1, "Vector2i", 0, 2, 0, 1, 1), module);
    scheme_add_global("Rect2", scheme_make_prim_w_everything(Scheme_Rect2_ctor, 1, "Rect2", 0, 4, 0, 1, 1), module);
    scheme_add_global("Rect2i", scheme_make_prim_w_everything(Scheme_Rect2i_ctor, 1, "Rect2i", 0, 4, 0, 1, 1), module);
    scheme_add_global("Vector3", scheme_make_prim_w_everything(Scheme_Vector3_ctor, 1, "Vector3", 0, 3, 0, 1, 1), module);
    scheme_add_global("Vector3i", scheme_make_prim_w_everything(Scheme_Vector3i_ctor, 1, "Vector3i", 0, 3, 0, 1, 1), module);
    scheme_add_global("Transform2D", scheme_make_prim_w_everything(Scheme_Transform2D_ctor, 1, "Transform2D", 0, 4, 0, 1, 1), module);
    scheme_add_global("Vector4", scheme_make_prim_w_everything(Scheme_Vector4_ctor, 1, "Vector4", 0, 4, 0, 1, 1), module);
    scheme_add_global("Vector4i", scheme_make_prim_w_everything(Scheme_Vector4i_ctor, 1, "Vector4i", 0, 4, 0, 1, 1), module);
    scheme_add_global("Plane", scheme_make_prim_w_everything(Scheme_Plane_ctor, 1, "Plane", 0, 4, 0, 1, 1), module);
    scheme_add_global("Quaternion", scheme_make_prim_w_everything(Scheme_Quaternion_ctor, 1, "Quaternion", 0, 4, 0, 1, 1), module);
    scheme_add_global("AABB", scheme_make_prim_w_everything(Scheme_AABB_ctor, 1, "AABB", 0, 2, 0, 1, 1), module);
    scheme_add_global("Basis", scheme_make_prim_w_everything(Scheme_Basis_ctor, 1, "Basis", 0, 3, 0, 1, 1), module);
    scheme_add_global("Transform3D", scheme_make_prim_w_everything(Scheme_Transform3D_ctor, 1, "Transform3D", 0, 4, 0, 1, 1), module);
    scheme_add_global("Projection", scheme_make_prim_w_everything(Scheme_Projection_ctor, 1, "Projection", 0, 4, 0, 1, 1), module);
    scheme_add_global("Color", scheme_make_prim_w_everything(Scheme_Color_ctor, 1, "Color", 0, 4, 0, 1, 1), module);
    scheme_add_global("RID", scheme_make_prim_w_everything(Scheme_RID_ctor, 1, "RID", 0, 1, 0, 1, 1), module);
    scheme_add_global("Callable", scheme_make_prim_w_everything(Scheme_Callable_ctor, 1, "Callable", 0, 2, 0, 1, 1), module);
    scheme_add_global("Signal", scheme_make_prim_w_everything(Scheme_Signal_ctor, 1, "Signal", 0, 2, 0, 1, 1), module);
    scheme_add_global("Dictionary", scheme_make_prim_w_everything(Scheme_Dictionary_ctor, 1, "Dictionary", 0, 1, 0, 1, 1), module);
    scheme_add_global("Array", scheme_make_prim_w_everything(Scheme_Array_ctor, 1, "Array", 0, 4, 0, 1, 1), module);
    scheme_add_global("PackedByteArray", scheme_make_prim_w_everything(Scheme_PackedByteArray_ctor, 1, "PackedByteArray", 0, 1, 0, 1, 1), module);
    scheme_add_global("PackedInt32Array", scheme_make_prim_w_everything(Scheme_PackedInt32Array_ctor, 1, "PackedInt32Array", 0, 1, 0, 1, 1), module);
    scheme_add_global("PackedInt64Array", scheme_make_prim_w_everything(Scheme_PackedInt64Array_ctor, 1, "PackedInt64Array", 0, 1, 0, 1, 1), module);
    scheme_add_global("PackedFloat32Array", scheme_make_prim_w_everything(Scheme_PackedFloat32Array_ctor, 1, "PackedFloat32Array", 0, 1, 0, 1, 1), module);
    scheme_add_global("PackedFloat64Array", scheme_make_prim_w_everything(Scheme_PackedFloat64Array_ctor, 1, "PackedFloat64Array", 0, 1, 0, 1, 1), module);
    scheme_add_global("PackedStringArray", scheme_make_prim_w_everything(Scheme_PackedStringArray_ctor, 1, "PackedStringArray", 0, 1, 0, 1, 1), module);
    scheme_add_global("PackedVector2Array", scheme_make_prim_w_everything(Scheme_PackedVector2Array_ctor, 1, "PackedVector2Array", 0, 1, 0, 1, 1), module);
    scheme_add_global("PackedVector3Array", scheme_make_prim_w_everything(Scheme_PackedVector3Array_ctor, 1, "PackedVector3Array", 0, 1, 0, 1, 1), module);
    scheme_add_global("PackedColorArray", scheme_make_prim_w_everything(Scheme_PackedColorArray_ctor, 1, "PackedColorArray", 0, 1, 0, 1, 1), module);
}
