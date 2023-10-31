#!/usr/bin/env python
import os
import sys

env = SConscript("godot-cpp/SConstruct")

sources = Glob("src/*.cpp") + Glob("src/**/*.cpp")

env.Append(CPPPATH=["src/", "src/binder", "src/util"])

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

if env["platform"] == "windows":
    env.Append(CCFLAGS=["/utf-8", "/EHsc"])
    env.Append(CPPDBFLAGS=["/Zi", "/Fd${TARGET}.pdb"])
    env.Append(CCFLAGS=["/Z7"])
    env.Append(LINKFLAGS=["/DEBUG"])

if env["platform"] == "macos":
    library = env.SharedLibrary(
        "demo/bin/libgdracket.{}.{}.framework/libgdracket.{}.{}".format(
            env["platform"], env["target"], env["platform"], env["target"]
        ),
        source=sources,
    )
else:
    library = env.SharedLibrary(
        "bin/libgdracket{}{}".format(env["suffix"], env["SHLIBSUFFIX"]),
        source=sources,
    )

Default(library)
