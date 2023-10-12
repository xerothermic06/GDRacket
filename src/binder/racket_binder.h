#ifndef GDRACKET_BINDER_H
#define GDRACKET_BINDER_H

#include <thread>
#include <mutex>
#include <condition_variable>

#include "godot_cpp/templates/hash_map.hpp"
#include "godot_cpp/templates/hash_set.hpp"
#include "godot_cpp/classes/thread.hpp"

#include "scheme_error.h"
#include "util/scheme_classes.h"
#include "scheme_binder.h"

#include "scheme.h"

// SchemeBinder for RacketBC.
class RacketBinder : public SchemeBinder {

    HashMap<uint32_t, Scheme_Env*> script_id_namespace_map;
    // Mapping from Script ID -> Racket symbol for module path
    HashMap<uint32_t, Scheme_Object*> script_id_class_map;
    // Mapping from Script ID -> class definition copy
    HashMap<uint32_t, GDClassDefinition> script_id_definition_map;
    // Mapping from ScriptInstance ID -> Racket object for instance
    HashMap<uint32_t, Scheme_Object*> instance_id_ctx_map;

protected:

public:
    enum EvalType {
        EVAL_TYPE_MODULE,
        EVAL_TYPE_GLOBAL,
    };

    RacketBinder();
    ~RacketBinder();

    Error create_definition(const SchemeScript &script, GDClassDefinition& def);
    GDClassDefinition* get_definition(const SchemeScript &script);
    void delete_definition(const SchemeScript &p_script);

    int32_t get_member_line(SchemeScriptInstance &p_target, const StringName &member);

    Error initialize_instance(SchemeScriptInstance &p_target);
    Variant call(SchemeScriptInstance &p_target, const String p_func_name, const Variant **p_args, int p_argcount, SchemeCallError* r_error);
	bool set(SchemeScriptInstance &p_target, const StringName &p_name, const Variant &p_value);
	bool get(const SchemeScriptInstance &p_target, const StringName, Variant *r_ret) const;
	bool has_method(const SchemeScriptInstance &p_target, const StringName &p_method) const;
    void free_instance(SchemeScriptInstance &p_target);

#ifdef TOOLS_ENABLED
    virtual const Dictionary &get_modified_api() const;
#endif
};

#endif // GDRACKET_BINDER_H
