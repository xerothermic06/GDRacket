#pragma once

#include "godot_cpp/variant/string.hpp"


// copy-paste of godot::Callable::CallError
class SchemeCallError {
public:
    enum Error {
        CALL_OK,
        CALL_ERROR_INVALID_METHOD,
        CALL_ERROR_INVALID_ARGUMENT, // expected is variant type
        CALL_ERROR_TOO_MANY_ARGUMENTS, // expected is number of arguments
        CALL_ERROR_TOO_FEW_ARGUMENTS, // expected is number of arguments
        CALL_ERROR_INSTANCE_IS_NULL,
        CALL_ERROR_METHOD_NOT_CONST,
    };
    Error error = Error::CALL_OK;
    int argument = 0;
    int expected = 0;
    godot::String message;
};

