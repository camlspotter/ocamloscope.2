#!/bin/sh
set -e
rm -rf *.cm*
ocamlc -c -no-alias-deps mylib.ml
ocamlc -c -no-alias-deps -open Mylib mylib__A.ml
ocamlc -c -no-alias-deps -open Mylib mylib__B.ml
ocamlc -o mylib.cma -a mylib.cmo mylib__A.cmo mylib__B.cmo 
