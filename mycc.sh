#/bin/env bash

# Compile C to assembly with mynqdcc, then call "gcc -w" to generate an executable.
# This is useful for running the nqcc test suite, e.g.:
# ```
# cd write_a_c_compiler
# ./test_compiler.sh "../mycc.sh /path/to/mynqcc-exe" 1
# ```


cmd="$1"
srcf="$2"
outf="${srcf%.*}"
asmf="${outf}.s"

$cmd -o "$asmf" "$srcf" && gcc -m32 -w -o "$outf" "$asmf"
