#include <godot_cpp/godot.hpp>

#include "scheme_script_instance.h"
#include "scheme_language.h"
#include "scheme_error.h"
#include "scheme_script.h"
#include <cstdio>

// ReSharper disable CppClangTidyMiscMisplacedConst
// ReSharper disable CppMemberFunctionMayBeConst

// Interface between plugin and GDExtension's native scripting interface.

GDExtensionScriptInstancePtr SchemeScriptInstance::create_instance(const SchemeScript* parent,
                                                                     godot::Object* host_object) {

    auto parent_ref = Ref<SchemeScript>(parent);
    auto inst = memnew(SchemeScriptInstance(parent_ref, host_object));

    {
        auto lock = parent->language->get_instance_lock();
        parent_ref->instances.insert(parent->get_instance_id(), inst);
    }

    auto instance_info = &SchemeScriptInstance::instance_info;

    GDExtensionScriptInstancePtr instPtr = godot::internal::gdextension_interface_script_instance_create(
        instance_info, inst);

    // if (parent->classInfo.create_instance_func != nullptr) {
    //     GDExtensionObjectPtr backingObject = parent->classInfo.create_instance_func(nullptr);
    //     inst->owner_object = backingObject; //std::make_unique<GDExtensionObjectPtr>(backingObject);
    // }

    return instPtr;
}


SchemeScriptInstance::SchemeScriptInstance(Ref<SchemeScript> script, godot::Object* owner_object):
    script(script), owner_object(owner_object) {
    SchemeLanguage::get_singleton()->binder->scheme_initialize_instance(*this);

}


SchemeScriptInstance::~SchemeScriptInstance() {
    std::cout << "ssi dtor" << std::endl;
    if (script.is_valid() && owner_object) {
        uint64_t object_id = owner_object->get_instance_id();
        auto lock = script->language->get_instance_lock();
        script->instances.erase(object_id);
        SchemeLanguage::get_singleton()->get_binder()->scheme_free_instance(*this);
    }
}


Ref<Script> SchemeScriptInstance::get_script() const {
    return script;
}


Variant SchemeScriptInstance::callp(
    const StringName &p_method,
    const Variant **p_args,
    int p_argcount,
    SchemeCallError &r_error) {
    return SchemeLanguage::get_singleton()->get_binder()->scheme_call(*this, p_method, p_args, p_argcount, &r_error);
}


bool SchemeScriptInstance::has_method(const StringName &p_method) const {
    auto nm = godot::StringName("test");
    return (p_method) == nm;
}


void SchemeScriptInstance::notification(int p_notification) {
    // TODO: collect error?
    const Variant* which = &Variant(p_notification);
    SchemeLanguage::get_singleton()->get_binder()->scheme_call(*this, "_notification", &which, 1, NULL);
}


bool SchemeScriptInstance::set(const StringName &p_name, const Variant &p_value) {
    return false;
}


bool SchemeScriptInstance::get(const StringName &p_name, Variant &r_ret) const {
    r_ret = Variant(1);
    return true;
}


void SchemeScriptInstance::get_property_list(List<PropertyInfo> *p_properties) const {
    p_properties->push_back(
        make_property_info(
        Variant::Type::INT,
        "test",
        PropertyHint::PROPERTY_HINT_NONE)
    );
}


void SchemeScriptInstance::get_method_list(List<MethodInfo> *p_list) const {
    p_list->push_back(MethodInfo("test"));
}


Variant::Type SchemeScriptInstance::get_property_type(const StringName &p_name, bool *r_is_valid) const {
    if (p_name != StringName("test")) {
        *r_is_valid = false;
        return Variant::Type::NIL;
    }
    return Variant::Type::INT;
}


ScriptLanguage* SchemeScriptInstance::get_language() {
    return script->language;
};


GDExtensionPropertyInfo _convert_prop_info(PropertyInfo &p_info) {
    GDExtensionPropertyInfo info = {};
    info.name = &p_info.name;
    info.type = (GDExtensionVariantType)p_info.type;
    info.class_name = &p_info.class_name;
    info.hint = p_info.hint;
    info.hint_string = &p_info.hint_string;
    info.usage = p_info.usage;
    return info;
}


GDExtensionMethodInfo _convert_method_info(MethodInfo info) {
    GDExtensionMethodInfo minfo = {};
    minfo.name = &info.name;
    minfo.return_value = _convert_prop_info(info.return_val);
    minfo.flags = info.flags;
    minfo.id = info.id;
    minfo.argument_count = info.arguments.size();
    minfo.arguments = (GDExtensionPropertyInfo*) std::malloc(minfo.argument_count * sizeof(GDExtensionPropertyInfo));
    for (int i = 0; i < minfo.argument_count; i++) {
        minfo.arguments[i] = _convert_prop_info(info.arguments[i]);
    }
    minfo.default_argument_count = info.default_arguments.size();
    for (int i = 0; i < minfo.default_argument_count; i++) {
        minfo.default_arguments[i] = &info.default_arguments[i];
    }
    return minfo;
}


GDExtensionCallError _convert_call_err(SchemeCallError p_err) {
    GDExtensionCallError gde_err;
    gde_err.error = (GDExtensionCallErrorType)p_err.error;
    gde_err.argument = p_err.argument;
    gde_err.expected = p_err.expected;
    return gde_err;
}


GDExtensionObjectPtr s_get_script(GDExtensionScriptInstanceDataPtr void_this) {
    auto script = ((reinterpret_cast<SchemeScriptInstance*>(void_this))->get_script());
    return script.ptr();
}


void s_free(GDExtensionScriptInstanceDataPtr void_this) {
    memdelete((SchemeScriptInstance*)void_this);
    // delete void_this;
}


GDExtensionBool s_has_method(GDExtensionScriptInstanceDataPtr p_instance, GDExtensionConstStringNamePtr p_name) {
    UtilityFunctions::print("s_has_method");
    return reinterpret_cast<SchemeScriptInstance*>(p_instance)->has_method(*reinterpret_cast<const StringName*>(p_name));
}


void s_call(
        GDExtensionScriptInstanceDataPtr p_instance,
        GDExtensionConstStringNamePtr p_method,
        const GDExtensionConstVariantPtr *p_args,
        GDExtensionInt p_argument_count,
        GDExtensionVariantPtr r_return,
        GDExtensionCallError *r_error
    ) {
    // if (p_argument_count > 0) {
    //     GDExtensionVariantType typ = godot::internal::gdextension_interface_variant_get_type(*p_args);
    //     printf("variant int: %d\n", typ);
    //     const Variant** args = (const Variant**)p_args;
    //     printf("variant cast int: %d\n", args[0]->get_type());
    // }
    auto args = (const Variant**)p_args; //reinterpret_cast<const Variant*>(p_args);
    auto method_name = *reinterpret_cast<const StringName*>(p_method);
    auto ret_ptr = reinterpret_cast<Variant*>(r_return);
    SchemeCallError ret_err;
    *ret_ptr = reinterpret_cast<SchemeScriptInstance*>(p_instance)->callp(
        method_name, args, p_argument_count, ret_err);
    *r_error = _convert_call_err(ret_err);
}


GDExtensionBool s_set(
    GDExtensionScriptInstanceDataPtr p_instance,
    GDExtensionConstStringNamePtr p_name,
    GDExtensionConstVariantPtr p_value) {
    return reinterpret_cast<SchemeScriptInstance*>(p_instance)->set(
        *reinterpret_cast<const StringName*>(p_name),
        *reinterpret_cast<const Variant*>(p_value));
}

GDExtensionBool s_get(
    GDExtensionScriptInstanceDataPtr p_instance,
    GDExtensionConstStringNamePtr p_name,
    GDExtensionVariantPtr r_ret) {
    auto ret = *reinterpret_cast<Variant*>(r_ret);
    return reinterpret_cast<SchemeScriptInstance*>(p_instance)->get(
        *reinterpret_cast<const StringName*>(p_name),
        ret
    );
}


const GDExtensionPropertyInfo* s_get_property_list(
    GDExtensionScriptInstanceDataPtr p_instance,
    uint32_t *r_count) {

    List<PropertyInfo> prop_info;
    reinterpret_cast<SchemeScriptInstance*>(p_instance)->get_property_list(&prop_info);
    int count = prop_info.size();
    GDExtensionPropertyInfo* gde_info = (GDExtensionPropertyInfo*) std::malloc( sizeof(GDExtensionPropertyInfo));
    for (int i = 0; i < count; i++) {
        gde_info[i] = _convert_prop_info(prop_info[i]);
    }
    *r_count = count;
    return gde_info;
}


void s_free_property_list(GDExtensionScriptInstanceDataPtr p_instance, const GDExtensionPropertyInfo *p_list) {
    delete p_list;
}

GDExtensionVariantType s_get_property_type(
    GDExtensionScriptInstanceDataPtr p_instance,
    GDExtensionConstStringNamePtr p_name,
    GDExtensionBool *r_is_valid) {

    auto prop_name = *reinterpret_cast<const StringName*>(p_name);
    auto is_valid = reinterpret_cast<bool*>(r_is_valid);
    auto prop_type = reinterpret_cast<SchemeScriptInstance*>(p_instance)->get_property_type(prop_name, is_valid);
    return (GDExtensionVariantType)prop_type;
}


void s_notification(GDExtensionScriptInstanceDataPtr p_instance, int32_t p_what) {
    reinterpret_cast<SchemeScriptInstance*>(p_instance)->notification(p_what);
}

GDExtensionScriptLanguagePtr s_get_language(GDExtensionScriptInstanceDataPtr p_instance) {
    return reinterpret_cast<SchemeScriptInstance*>(p_instance)->get_language();
}


GDExtensionObjectPtr s_get_owner(GDExtensionScriptInstanceDataPtr p_instance) {
    return reinterpret_cast<SchemeScriptInstance*>(p_instance)->get_owner();
}


const GDExtensionMethodInfo* s_get_method_list(GDExtensionScriptInstanceDataPtr p_instance, uint32_t *r_count) {
    List<MethodInfo> p_list;
    reinterpret_cast<SchemeScriptInstance*>(p_instance)->get_method_list(&p_list);

    auto sz = (p_list.size());
    GDExtensionMethodInfo* m_info = (GDExtensionMethodInfo*) std::malloc(sz * sizeof(GDExtensionMethodInfo));
    for (int i = 0; i < sz; i++) {
        m_info[i] = _convert_method_info(p_list[i]);
    }
    *r_count = (uint32_t)sz;
    return m_info;
}


void s_free_method_list(GDExtensionScriptInstanceDataPtr p_instance, const GDExtensionMethodInfo *p_list) {
    delete p_list;
}


GDExtensionScriptInstanceInfo init_instance_info() {
    GDExtensionScriptInstanceInfo instance_info;

    instance_info.set_func = s_set;
    instance_info.get_func = s_get;

    instance_info.get_property_list_func = s_get_property_list;
    instance_info.free_property_list_func = s_free_property_list;

    instance_info.property_can_revert_func = []
        (GDExtensionScriptInstanceDataPtr p_instance, GDExtensionConstStringNamePtr p_name)
        -> GDExtensionBool {return 0;};
    instance_info.property_get_revert_func = []
        (GDExtensionScriptInstanceDataPtr p_instance, GDExtensionConstStringNamePtr p_name, GDExtensionVariantPtr r_ret)
        -> GDExtensionBool {return 0;};

    instance_info.get_owner_func = s_get_owner;
    instance_info.get_property_state_func = []
        (GDExtensionScriptInstanceDataPtr p_instance, GDExtensionScriptInstancePropertyStateAdd p_add_func, void *p_userdata)
        -> void { };

    instance_info.get_method_list_func = s_get_method_list;
    instance_info.free_method_list_func = s_free_method_list;

    instance_info.get_property_type_func = s_get_property_type;
    instance_info.has_method_func = s_has_method;
    instance_info.call_func = s_call;
    instance_info.notification_func = s_notification;

    instance_info.get_language_func = s_get_language;
    instance_info.to_string_func = []
        (GDExtensionScriptInstanceDataPtr p_instance, GDExtensionBool *r_is_valid, GDExtensionStringPtr r_out)
        -> void {
            r_out = memnew(String("scheme script"));
        };
    instance_info.refcount_incremented_func = []
        (GDExtensionScriptInstanceDataPtr p_instance)
        -> void {};
    instance_info.refcount_decremented_func = []
        (GDExtensionScriptInstanceDataPtr p_instance)
        -> GDExtensionBool {
            return 1;
        };
    instance_info.get_script_func = s_get_script;
    instance_info.is_placeholder_func = []
        (GDExtensionScriptInstanceDataPtr p_instance)
        -> GDExtensionBool { return 0; };
    instance_info.set_fallback_func = []
        (GDExtensionScriptInstanceDataPtr p_instance, GDExtensionConstStringNamePtr p_name, GDExtensionConstVariantPtr p_value)
        -> GDExtensionBool { return 0; };
    instance_info.get_fallback_func = []
        (GDExtensionScriptInstanceDataPtr p_instance, GDExtensionConstStringNamePtr p_name, GDExtensionVariantPtr r_ret)
        -> GDExtensionBool { return 0; };
    instance_info.free_func = s_free;
    return instance_info;
}

GDExtensionScriptInstanceInfo SchemeScriptInstance::instance_info = init_instance_info();
