#!/bin/bash
# TODO: redo this for scons
raco="/d/projects/external/racket/racket/racoBC.exe"
dest="./src/binder/racket_modules.c"

$raco ctool --c-mods "$dest" \
    ++lib racket/main \
    ++lib racket/base \
    ++lib racket/exn \
    ++lib racket/class \
    ++lib racket/vector \
    ++lib racket/lang/reader \
    ++lib racket/runtime-config
