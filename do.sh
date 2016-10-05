refmt -parse re -print ml src/main.re > src/main.ml
make
echo "============================"
_build/src/main.native | refmt -use-stdin true -parse binary -print re -is-interface-pp false
