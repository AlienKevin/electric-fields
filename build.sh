#!/bin/sh

set -e

js="public/elm.js"
min="public/elm.min.js"

sed -i 's+/public/elm.js+elm.js+' public/index.html

elm make --optimize --output=$js "src/Main.elm"

terser $js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | terser --mangle --output=$min

echo "Compiled size:$(cat $js | wc -c) bytes"
echo "Minified size:$(cat $min | wc -c) bytes"
echo "Gzipped size: $(cat $min | gzip -c | wc -c) bytes"

mv public/elm.min.js public/elm.js