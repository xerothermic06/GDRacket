#include <godot_cpp/godot.hpp>

#include "racket_script_instance.h"
#include "racket_language.h"
#include "racket_error.h"
#include "racket_script.h"
#include <cstdio>


GDExtensionScriptInstancePtr RacketScriptInstance::create_instance(const RacketScript* parent,
                                                                     godot::Object* host_object) {
    Ref<RacketScript> parent_ref = Ref<RacketScript>(parent);
    RacketScriptInstance* inst = memnew(RacketScriptInstance(parent_ref, host_object));

    {
        MutexLock lock = parent->language->get_instance_lock();
        parent_ref->instances.insert(parent->get_instance_id(), inst);
    }

    const GDExtensionScriptInstanceInfo2* instance_info =
        const_cast<GDExtensionScriptInstanceInfo2*>(&RacketScriptInstance::instance_info);

    GDExtensionScriptInstancePtr instPtr =
        godot::internal::gdextension_interface_script_instance_create2(instance_info, inst);

    return instPtr;
}


RacketScriptInstance::RacketScriptInstance(Ref<RacketScript> script, godot::Object* owner_object):
    script(script), owner_object(owner_object) {
    RacketLanguage::get_singleton()->get_binder()->initialize_instance(*this);
}


RacketScriptInstance::~RacketScriptInstance() {
    std::cout << "ssi dtor" << std::endl;
    if (script.is_valid() && owner_object) {
        uint64_t object_id = owner_object->get_instance_id();
        auto lock = script->language->get_instance_lock();
        script->instances.erase(object_id);
        RacketLanguage::get_singleton()->get_binder()->free_instance(*this);
    }
}


Ref<RacketScript> RacketScriptInstance::get_script() const {
    return script;
}


Variant RacketScriptInstance::callp(
    const StringName &p_method,
    const Variant **p_args,
    int p_argcount,
    RacketCallError &r_error) {
    return RacketLanguage::get_singleton()->get_binder()->call(*this, p_method, p_args, p_argcount, &r_error);
}


bool RacketScriptInstance::has_method(const StringName &p_method) const {
    auto classDef = RacketLanguage::get_singleton()->get_binder()->get_definition(*(get_script().ptr()));
    if (classDef == nullptr) {
        return false;
    }
    return classDef->has_method(p_method);
}


void RacketScriptInstance::notification(int p_notification) {
    // TODO: collect error?
    const Variant* which = &Variant(p_notification);
    RacketLanguage::get_singleton()->get_binder()->call(*this, "_notification", &which, 1, NULL);
}


bool RacketScriptInstance::set(const StringName &p_name, const Variant &p_value) {
    return RacketLanguage::get_singleton()->get_binder()->set(*this, p_name, p_value);
}


bool RacketScriptInstance::get(const StringName &p_name, Variant *r_ret) const {
    return RacketLanguage::get_singleton()->get_binder()->get(*this, p_name, r_ret);
}

// TODO: cache values for property and method lists in RacketScript instead
void RacketScriptInstance::get_property_list(List<PropertyInfo> *p_properties) const {
    GDClassDefinition* def = RacketLanguage::get_singleton()->get_binder()->get_definition(*this->script.ptr());
    for (int i = 0; i < def->properties.size(); i++) {
        p_properties->push_back((PropertyInfo)(def->properties[i]));
    }
}


void RacketScriptInstance::get_method_list(List<MethodInfo> *p_methods) const {
    GDClassDefinition* def = RacketLanguage::get_singleton()->get_binder()->get_definition(*this->script.ptr());
    for (auto iter : def->methods) {
        p_methods->push_back((MethodInfo)iter.value);
    }
}


Variant::Type RacketScriptInstance::get_property_type(const StringName &p_name, bool *r_is_valid) const {
    GDClassDefinition* def = RacketLanguage::get_singleton()->get_binder()->get_definition(*this->script.ptr());
    return (Variant::Type)def->properties[def->property_indices[p_name]].property.type;
}


ScriptLanguage* RacketScriptInstance::get_language() {
    return script->language;
};


// GDExtension functions ///////////////////////////////////////////////////////

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


GDExtensionCallError _convert_call_err(RacketCallError p_err) {
    GDExtensionCallError gde_err;
    gde_err.error = (GDExtensionCallErrorType)p_err.error;
    gde_err.argument = p_err.argument;
    gde_err.expected = p_err.expected;
    return gde_err;
}


GDExtensionObjectPtr s_get_script(GDExtensionScriptInstanceDataPtr void_this) {
    auto script = ((reinterpret_cast<RacketScriptInstance*>(void_this))->get_script());
    return script.ptr();
}


void s_free(GDExtensionScriptInstanceDataPtr void_this) {
    memdelete((RacketScriptInstance*)void_this);
}


GDExtensionBool s_has_method(GDExtensionScriptInstanceDataPtr p_instance, GDExtensionConstStringNamePtr p_name) {
    return reinterpret_cast<RacketScriptInstance*>(p_instance)->has_method(*reinterpret_cast<const StringName*>(p_name));
}


void s_call(
        GDExtensionScriptInstanceDataPtr p_instance,
        GDExtensionConstStringNamePtr p_method,
        const GDExtensionConstVariantPtr *p_args,
        GDExtensionInt p_argument_count,
        GDExtensionVariantPtr r_return,
        GDExtensionCallError *r_error
    ) {
    auto args = (const Variant**)p_args;
    auto method_name = *reinterpret_cast<const StringName*>(p_method);
    auto ret_ptr = reinterpret_cast<Variant*>(r_return);
    RacketCallError ret_err;
    *ret_ptr = reinterpret_cast<RacketScriptInstance*>(p_instance)->callp(
        method_name, args, p_argument_count, ret_err);
    *r_error = _convert_call_err(ret_err);
}


GDExtensionBool s_set(
    GDExtensionScriptInstanceDataPtr p_instance,
    GDExtensionConstStringNamePtr p_name,
    GDExtensionConstVariantPtr p_value) {
    return reinterpret_cast<RacketScriptInstance*>(p_instance)->set(
        *reinterpret_cast<const StringName*>(p_name),
        *reinterpret_cast<const Variant*>(p_value));
}

GDExtensionBool s_get(
    GDExtensionScriptInstanceDataPtr p_instance,
    GDExtensionConstStringNamePtr p_name,
    GDExtensionVariantPtr r_ret) {
    auto ret = reinterpret_cast<Variant*>(r_ret);
    return reinterpret_cast<RacketScriptInstance*>(p_instance)->get(
        *reinterpret_cast<const StringName*>(p_name), ret
    );
}


const GDExtensionPropertyInfo* s_get_property_list(
    GDExtensionScriptInstanceDataPtr p_instance,
    uint32_t *r_count) {

    List<PropertyInfo> prop_info;
    reinterpret_cast<RacketScriptInstance*>(p_instance)->get_property_list(&prop_info);
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
    auto prop_type = reinterpret_cast<RacketScriptInstance*>(p_instance)->get_property_type(prop_name, is_valid);
    return (GDExtensionVariantType)prop_type;
}


void s_notification(GDExtensionScriptInstanceDataPtr p_instance, int32_t p_what, GDExtensionBool p_reserved) {
    reinterpret_cast<RacketScriptInstance*>(p_instance)->notification(p_what);
}


GDExtensionScriptLanguagePtr s_get_language(GDExtensionScriptInstanceDataPtr p_instance) {
    return reinterpret_cast<RacketScriptInstance*>(p_instance)->get_language();
}


GDExtensionObjectPtr s_get_owner(GDExtensionScriptInstanceDataPtr p_instance) {
    return reinterpret_cast<RacketScriptInstance*>(p_instance)->get_owner();
}


const GDExtensionMethodInfo* s_get_method_list(GDExtensionScriptInstanceDataPtr p_instance, uint32_t *r_count) {
    List<MethodInfo> p_list;
    reinterpret_cast<RacketScriptInstance*>(p_instance)->get_method_list(&p_list);

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


GDExtensionScriptInstanceInfo2 init_instance_info() {
    GDExtensionScriptInstanceInfo2 instance_info;

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

GDExtensionScriptInstanceInfo2 RacketScriptInstance::instance_info = init_instance_info();
