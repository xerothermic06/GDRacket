#!/usr/bin/env python
import os
import sys

env = SConscript("godot-cpp/SConstruct")

SCHEME_ENGINE = "chibi-scheme"

# For reference:
# - CCFLAGS are compilation flags shared between C and C++
# - CFLAGS are for C-specific compilation flags
# - CXXFLAGS are for C++-specific compilation flags
# - CPPFLAGS are for pre-processor flags
# - CPPDEFINES are for pre-processor defines
# - LINKFLAGS are for linking flags

# tweak this if you want to use different folders, or more folders, to store your source code in.
env.Append(CPPPATH=["src/", "src/binder", "src/util"])
sources = Glob("src/*.cpp") + Glob("src/**/*.cpp")
# sources = Glob("src/*.cpp", "src/**/*.cpp")
env.Append(CCFLAGS=["/utf-8"])

env.Append(CPPDBFLAGS=["/Zi", "/Fd${TARGET}.pdb"])
env.Append(CCFLAGS=["/Z7"])
env.Append(LINKFLAGS=["/DEBUG"])

if SCHEME_ENGINE == "chibi-scheme":
    env.Append(CPPPATH=["scheme/chibi-scheme/include"])
    env.Append(CPPDEFINES=["BUILDING_DLL", "SEXP_USE_MODULES=1"])

    # chibi must link to winsock2 which doesn't seem to be in the lib paths by default during linking
    env.Append(LIBS=["ws2_32"])
    sources += Glob("scheme/chibi-scheme/*.c", exclude=[ 'scheme\chibi-scheme\plan9*', '*process*' ])

env.Append(CPPPATH=["scheme/racket/racket/src/bc"])
env.Append(CPPPATH=["scheme/racket/racket/src/bc/include"])
env.Append(CPPPATH=["scheme/racket/racket/src/bc/src"])
env.Append(CPPPATH=["scheme/racket/racket/src/bc/rktio"])
env.Append(CPPPATH=["scheme/racket/racket/src/rktio"])
env.Append(CPPPATH=["scheme/racket/racket/src/worksp"])
# scheme\racket\racket\src\bc\lib\libracket3mxxxxxxx.lib
env.Append(LIBS=[
    # "scheme/racket/racket/src/bc/lib/libracket3mxxxxxxx.lib",
    "scheme/racket/racket/src/bc/lib/libracketxxxxxxx.lib",
    "scheme/racket/racket/src/bc/lib/libmzgcxxxxxxx.lib",
    "scheme/racket/racket/src/bc/rktio/librktio.lib",
])
# sources += "./base.c"


if env["platform"] == "macos":
    library = env.SharedLibrary(
        "demo/bin/libgdscheme.{}.{}.framework/libgdscheme.{}.{}".format(
            env["platform"], env["target"], env["platform"], env["target"]
        ),
        source=sources,
    )
else:
    library = env.SharedLibrary(
        "bin/libgdscheme{}{}".format(env["suffix"], env["SHLIBSUFFIX"]),
        source=sources,
    )

Default(library)

# # Testing stuff
# test_src = Glob("scheme/chibi-scheme/*.c",
#     exclude=[ 'scheme\chibi-scheme\plan9*', '*process*', "scheme\chibi-scheme\main*" ]) + ["chibi_test.cpp"]
# # test_src = test_src + Glob("src/binder/racket_modules.c")
# pgm = env.Program(
#     "chibi_test",
#     source=test_src
# )
# Default(pgm)
