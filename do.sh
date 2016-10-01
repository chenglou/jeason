../Reason/refmt_impl.native -parse re -print ml src/main.re > src/main.ml
make
_build/src/main.native | ../Reason/refmt_impl.native -use-stdin true -parse binary -print re -is-interface-pp false
