#pragma once
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
// #include "schpriv.h"


enum RequestState {
    CREATED,
    PENDING,
    ERROR,
    COMPLETE
};


enum RequestType {
    STRING,
    SEXPR,
    FUNCTION
};

typedef Scheme_Object* (RacketMapper)(Scheme_Object*, Scheme_Env*);

class RacketRequest {

    friend class RacketEnvironment;

    RequestType request_type;
    RequestState state = RequestState::CREATED;
    std::string error_message;
    Scheme_Object* result;

    Scheme_Env* ns;
    std::string string_request;
    Scheme_Object* sexpr_request;
    RacketMapper* func_request;
    // Scheme_Object* (*mapper)(Scheme_Env*);

public:
    RacketRequest() {};
};


class RacketEnvironment {
    Scheme_Env* env;
    std::thread* worker;
    std::mutex* mutex;
    std::condition_variable* sem;
    bool ready;
    bool setup_done;
    RacketRequest request;

public:
    RacketEnvironment();
    Scheme_Env* get_env() { return env; }
    void set_env(Scheme_Env* e) { env = e; }
    Scheme_Object* eval(Scheme_Object* eval_list);
    Scheme_Object* eval_in(Scheme_Object* eval_list, Scheme_Env* ns);
    Scheme_Object* eval_string(std::string eval_str);
    Scheme_Object* eval_string_in(std::string eval_str, Scheme_Env* ns);
    Scheme_Object* eval_func_in(RacketMapper* mapper_func, Scheme_Object* args, Scheme_Env* ns);

    template<typename... T>
    Scheme_Object* eval_sexpr_v(T... ts) {
        const int size = sizeof...(ts);
        static_assert(size > 0);
        Scheme_Object* rest[size] = {ts...};
        Scheme_Object* list = scheme_build_list(size, rest);
        return eval(list);
    }

};


// Root interface for specific scheme implementations.
class RacketBinder : public SchemeBinder {

    RacketEnvironment* environment;
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


    RacketBinder() {}
    ~RacketBinder() {};

    void initialize();
    void uninitialize();

    GDClassDefinition scheme_create_definition(const SchemeScript &script);
    int32_t scheme_get_member_line(SchemeScriptInstance &p_target, const StringName &member);

    void scheme_initialize_instance(SchemeScriptInstance &p_target);
    Variant scheme_call(SchemeScriptInstance &p_target, const String p_func_name, const Variant **p_args, int p_argcount, SchemeCallError* r_error);
	bool set(SchemeScriptInstance &p_target, const StringName &p_name, const Variant &p_value);
	bool get(const SchemeScriptInstance &p_target, const StringName, Variant &r_ret) const;
	bool has_method(const SchemeScriptInstance &p_target, const StringName &p_method) const;
    void scheme_free_instance(SchemeScriptInstance &p_target);

#ifdef TOOLS_ENABLED
    virtual const Dictionary &get_modified_api() const;
#endif
};

// template<typename... Ts>
// void func(Ts... args)
// {
//     const int size = sizeof...(args) + 2;
//     int res[size] = {1, args..., 2};

//     // since initializer lists guarantee sequencing, this can be used to
//     // call a function on each element of a pack, in order:
//     int dummy[sizeof...(Ts)] = {(std::cout << args, 0)...};
// }

// template<typename... T>
// Scheme_Object* _scheme_eval(Scheme_Env* env, T... ts) {
//     const int size = sizeof...(ts);
//     Scheme_Object* res[size] = {ts...};
//     Scheme_Object* list = scheme_build_list(size, res);
//     return scheme_eval(list, env);
// }

#endif // GDRACKET_BINDER_H
