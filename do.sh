# utility script to check what the ocaml ast looks like
# ocamlc -pp refmt -dparsetree -impl dparse.re

make
echo "============================"
_build/src/main.native | refmt -use-stdin true -parse binary -print re -is-interface-pp false
