#!/bin/bash
set -x
cd $(dirname $0)
rm -f a.out.*


# Note that _testfun corresponds to testfun() in the .c file.

emcc -O3 \
     -s WASM=1 \
     -s ASSERTIONS=1 \
     -s EXPORTED_FUNCTIONS='["_testfun", "_main"]' \
     -s EXTRA_EXPORTED_RUNTIME_METHODS='["cwrap"]' \
     test.c


#     


# emcc -O3 -s WASM=1 -s ONLY_MY_CODE=1 test.c
# emcc -O3 -s WASM=1 -s SIDE_MODULE=1 test.c

wasm2wat a.out.wasm > a.out.wat
wasm-objdump -d a.out.wasm  > a.out.dump

# apt install jsbeautifier
js-beautify a.out.js > a.out.verbose.js

