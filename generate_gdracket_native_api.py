import json
import sys
from textwrap import indent, dedent

# Generates builtin and class boilerplate C++ code for primitive Racket
# procedures.

obr = "{"
cbr = "}"
nl = "\n"

def builtin_name_to_variant_type(builtin_name):
    return {
        "nil"                  : "NIL",
        "bool"                 : "BOOL",
        "int"                  : "INT",
        "float"                : "FLOAT",
        "String"               : "STRING",
        "Vector2"              : "VECTOR2",
        "Vector2i"             : "VECTOR2I",
        "Rect2"                : "RECT2",
        "Rect2i"               : "RECT2I",
        "Vector3"              : "VECTOR3",
        "Vector3i"             : "VECTOR3I",
        "Transform2D"          : "TRANSFORM2D",
        "Vector4"              : "VECTOR4",
        "Vector4i"             : "VECTOR4I",
        "Plane"                : "PLANE",
        "Quaternion"           : "QUATERNION",
        "AABB"                 : "AABB",
        "Basis"                : "BASIS",
        "Transform3D"          : "TRANSFORM3D",
        "Projection"           : "PROJECTION",
        "Color"                : "COLOR",
        "StringName"           : "STRING_NAME",
        "NodePath"             : "NODE_PATH",
        "RID"                  : "RID",
        "Object"               : "OBJECT",
        "Callable"             : "CALLABLE",
        "Signal"               : "SIGNAL",
        "Dictionary"           : "DICTIONARY",
        "Array"                : "ARRAY",
        "PackedByteArray"      : "PACKED_BYTE_ARRAY",
        "PackedInt32Array"     : "PACKED_INT32_ARRAY",
        "PackedInt64Array"     : "PACKED_INT64_ARRAY",
        "PackedFloat32Array"   : "PACKED_FLOAT32_ARRAY",
        "PackedFloat64Array"   : "PACKED_FLOAT64_ARRAY",
        "PackedStringArray"    : "PACKED_STRING_ARRAY",
        "PackedVector2Array"   : "PACKED_VECTOR2_ARRAY",
        "PackedVector3Array"   : "PACKED_VECTOR3_ARRAY",
        "PackedColorArray"     : "PACKED_COLOR_ARRAY",
        # VARIANT_MAX
    }.get(builtin_name, "NIL")


def wrapper_name_for_builtin(builtin_name):
    if builtin_name == "Object":
        return "Scheme_GodotObject"
    return f"Scheme_{builtin_name}"


def scheme_object_ptr_to_wrapper_ptr__cast(builtin_name, arg):
    if builtin_name == 'Variant':
        pass
    return f"({wrapper_name_for_builtin(builtin_name)}*)"


class BuiltinWrapperGenerator:

    @classmethod
    def main(cls, builtin_objs):
        filtered_builtin_objs = builtin_objs
        chunks = [cls.header()]
        for builtin_obj in filtered_builtin_objs:
            wrapper_type_name = wrapper_name_for_builtin(builtin_obj.get("name"))
            chunks.append(cls.builtin_wrapper_branching_ctor(builtin_obj, wrapper_type_name))
        chunks.append(cls.register_builtin_ctors(filtered_builtin_objs))
        return "\n".join(chunks)

    @classmethod
    def header(cls):
        return dedent(f"""
        #include "./racket_builtin_binder.h"
        #include "./racket_binder_util.h"

        using namespace godot;

        """).strip()


    @classmethod
    def builtin_wrapper_mapped_ctors(cls, builtin_type_obj, wrapper_type_name):
        builtin_type = builtin_type_obj['name']
        ctors = []
        for ctor_obj in builtin_type_obj["constructors"]:
            idx = ctor_obj.get('index', '')
            args_decl_list = ",".join([
                f"{arg.get('type')} {arg.get('name')}"
                for arg in ctor_obj.get('arguments', [])
            ])
            args_list = ",".join([
                arg.get('name')
                for arg in ctor_obj.get('arguments', [])
            ])

            ctors.append(dedent(f"""
            __FORCE_INLINE__ {wrapper_type_name}* {wrapper_type_name}_ctor_{idx}({args_decl_list}) {obr}
                {wrapper_type_name}* obj = scheme_malloc_allow_interior(sizeof({wrapper_type_name}));
                obj->v = {builtin_type}({args_list});
                return obj;
            {cbr}
            """))
        return "\n".join(ctors)


    @classmethod
    def builtin_wrapper_branching_ctor(cls, builtin_type_obj, wrapper_type_name):
        def type_check_clause(builtin_type, a, b):
            return {
                "bool": f"SCHEME_BOOLP({a})",
                "int": f"SCHEME_INTP({a})",
                "float": f"SCHEME_FLOATP({a})",
                "String": f"SCHEME_CHAR_STRINGP({a})",
                "Variant": f"binder->builtin_wrapperp({a})"
            }.get(builtin_type, f"binder->get_variant_type({a}) == Variant::Type::{builtin_name_to_variant_type(b)}")
        def arg_converter(builtin_type, arg):
            return {
                "bool": f"SCHEME_BOOL_VAL({arg})",
                "int": f"SCHEME_INT_VAL({arg})",
                "float": f"SCHEME_FLOAT_VAL({arg})",
                "String": f"rktstr2gdstr({arg})",
                "Variant": f"binder->scheme_object_to_variant({arg})"
            }.get(builtin_type, f"(({wrapper_name_for_builtin(builtin_type)}*){arg})->v")

        branches = []
        builtin_type = builtin_type_obj["name"]
        ctors_list = builtin_type_obj["constructors"]
        for idx, ctor in enumerate(ctors_list):
            block_header = "if" if idx == 0 else "else if"
            clauses = [f"argc == {len(ctor.get('arguments', []))}"]
            derefs = []
            for arg_idx, arg in enumerate(ctor.get('arguments', [])):
                arg_type = arg['type']
                clauses.append(type_check_clause(arg_type, f"argv[{arg_idx}]", arg_type))
                derefs.append(f'{arg_converter(arg_type, f"argv[{arg_idx}]")}')

            branches.append(indent(dedent(f"""
            {block_header} ({" && ".join(clauses)}) {obr}
                {wrapper_type_name}* obj = binder->new_builtin_instance<{wrapper_type_name}>();
                obj->v = {builtin_type}({", ".join(derefs)});
                return (Scheme_Object*)obj;
            {cbr}"""), "    " * 3))

        return dedent(f"""
        static Scheme_Object* {wrapper_type_name}_ctor(int argc, Scheme_Object** argv) {obr}
            BuiltinBinder* binder = BuiltinBinder::get_singleton();
            {nl.join(branches)}
            return nullptr;
        {cbr}""")


    @classmethod
    def register_ctor_line(cls, ctor_name, builtin_type, module_id, minargs, maxargs):
        return f'scheme_add_global("{builtin_type}", scheme_make_prim_w_everything({ctor_name}, 1, "{builtin_type}", {minargs}, {maxargs}, 0, 1, 1), {module_id});'


    @classmethod
    def register_builtin_ctors(cls, builtin_objs):
        lines = []
        for builtin_obj in builtin_objs:
            ctors_obj = builtin_obj.get("constructors", {})
            arg_counts = [len(ctor_obj.get("arguments", [])) for ctor_obj in ctors_obj]
            minargs, maxargs = min(arg_counts), max(arg_counts)
            builtin_name = builtin_obj.get('name')
            wrapper_name = wrapper_name_for_builtin(builtin_name)# f"Scheme_{builtin_name}"
            ctor_name = f"{wrapper_name}_ctor"
            lines.append(cls.register_ctor_line(ctor_name, builtin_name, 'module', minargs, maxargs))
        lines = indent("\n".join(lines), "    ")
        return dedent("""void register_builtin_wrapper_ctors(Scheme_Env* module) {{\n{lines}\n}}""".format(lines=lines))


class BuiltinWrapperPredicateGenerator:

    PREDICATE_TEMPLATE = "gdobject_isp"

    @classmethod
    def register_predicate_line(cls, builtin_type, module_id):
        return f'scheme_add_global("{builtin_type}?", scheme_make_prim_w_everything({cls.PREDICATE_TEMPLATE}<{builtin_type}>, 1, "{builtin_type}?", 1, 1, 0, 1, 1), {module_id});'

    @classmethod
    def main(cls, builtin_objs):
        lines = []
        for builtin_obj in builtin_objs:
            builtin_name = builtin_obj.get('name')
            wrapper_name = wrapper_name_for_builtin(builtin_name)
            lines.append(cls.register_predicate_line(builtin_name, 'module'))
        lines = "\n".join(lines)
        return ("""void register_builtin_predicates(Scheme_Env* module) {{\n{lines}\n}}""".format(lines=indent(lines, "    ")))


builtins_include_names = [
    "Vector2",
    "Vector2i",
    "Rect2",
    "Rect2i",
    "Vector3",
    "Vector3i",
    "Transform2D",
    "Vector4",
    "Vector4i",
    "Plane",
    "Quaternion",
    "AABB",
    "Basis",
    "Transform3D",
    "Projection",
    "Color",
    # "StringName",
    # "NodePath",
    "RID",
    # "ObjectID",
    # "GodotObject",
    "Callable",
    "Signal",
    "Dictionary",
    "Array",
    "PackedByteArray",
    "PackedInt32Array",
    "PackedInt64Array",
    "PackedFloat32Array",
    "PackedFloat64Array",
    "PackedStringArray",
    "PackedVector2Array",
    "PackedVector3Array",
    "PackedColorArray",
]


def main():
    with open(sys.argv[1]) as file:
        data = json.load(file)
        builtin_classes = [obj for obj in data["builtin_classes"] if obj.get("name") in builtins_include_names]

        ctors = BuiltinWrapperGenerator.main(builtin_classes) #builtin_wrapper_ctors(data["builtin_classes"][6])
        print(ctors)


if __name__ == '__main__':
    main()
