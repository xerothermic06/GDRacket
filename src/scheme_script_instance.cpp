#include <godot_cpp/godot.hpp>

#include "scheme_script_instance.h"
#include "scheme_language.h"
#include "scheme_error.h"
#include "scheme_script.h"

// ReSharper disable CppClangTidyMiscMisplacedConst
// ReSharper disable CppMemberFunctionMayBeConst

// Interface between plugin and GDExtension's native scripting interface.

GDExtensionScriptInstanceInfo SchemeScriptInstanceGlue::instance_info;

GDExtensionScriptInstancePtr SchemeScriptInstance::create_instance(const SchemeScript* parent,
                                                                     godot::Object* host_object) {
    // return godot::internal::gdn_interface->script_instance_create(
    //     &SchemeScriptInstanceGlue::instance_info,
    //     new SchemeScriptInstance(parent, host_object));

    // auto script_instance_create =
    auto parent_ref = Ref<SchemeScript>(parent);
    auto inst = new SchemeScriptInstance(Ref<SchemeScript>(parent), host_object);

    {
        // auto lock(*SchemeLanguage::singleton->instance_lock.ptr());
        auto lock = parent->language->get_instance_lock();
        parent_ref->instances.insert(parent->get_instance_id(), inst);
    }

    auto instPtr = godot::internal::gdextension_interface_script_instance_create(
        &SchemeScriptInstanceGlue::get_instance_info(), inst);

    // inst->owner_object = host_object;

    UtilityFunctions::print("scriptinstance created");

    // if (parent->classInfo.create_instance_func != nullptr) {
    //     GDExtensionObjectPtr backingObject = parent->classInfo.create_instance_func(nullptr);
    //     inst->owner_object = backingObject; //std::make_unique<GDExtensionObjectPtr>(backingObject);
    // }

    return instPtr;
}


SchemeScriptInstance::SchemeScriptInstance(Ref<SchemeScript> script, godot::Object* owner_object):
    script(script), owner_object(owner_object) {}


SchemeScriptInstance::~SchemeScriptInstance() {
    UtilityFunctions::print_verbose("script", script);
    if (script.is_valid() && owner_object) {

        uint64_t object_id = owner_object->get_instance_id();
        auto lock = script->language->get_instance_lock();
        script->instances.erase(object_id);
        auto self = this;
        binder->scheme_free(object_id);
    }
}

    // script = parent; //Ref<SchemeScript>(parent);
    // owner_object = host_object;

    // auto inst = new SchemeScriptInstance(parent, host_object);
    // auto instPtr = godot::internal::gdextension_interface_script_instance_create(
        // &SchemeScriptInstanceGlue::get_instance_info(), this);

    // inst->owner_object = host_object;

    // return instPtr;
// }


Ref<Script> SchemeScriptInstance::get_script() const {
    return script;
}


Variant SchemeScriptInstance::callp(
    const StringName &p_method, const Variant **p_args, int p_argcount, SchemeCallError &r_error) {
    UtilityFunctions::print("callp");
    return binder->scheme_call(*this, p_args, p_argcount, r_error);
}


bool SchemeScriptInstance::has_method(const StringName &p_method) const {
    auto nm = godot::StringName("test");
    return (p_method) == nm;
}

bool SchemeScriptInstance::set(const StringName &p_name, const Variant &p_value) {
    UtilityFunctions::print("set");
    return false;
}

bool SchemeScriptInstance::get(const StringName &p_name, Variant &r_ret) const {
    UtilityFunctions::print("Get");
    r_ret = Variant(1);
    return true;
}

void SchemeScriptInstance::get_property_list(List<PropertyInfo> *p_properties) const {
    UtilityFunctions::print("Get property list");
    p_properties->push_back(
        make_property_info(
        Variant::Type::INT,
        "test",
        PropertyHint::PROPERTY_HINT_NONE)
    );
}

void SchemeScriptInstance::get_method_list(List<MethodInfo> *p_list) const {
    UtilityFunctions::print("Get method list");
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
