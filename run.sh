#/bin/env bash

cmd="$1"
srcf="$2"
outf="${srcf%.*}"
asmf="${outf}.s"

$cmd "$srcf" && gcc -w -o "$outf" "$asmf"
