#!/bin/bash

mkdir -p tmp

rm tmp/input.tdiag


cat $1 >> tmp/input.tdiag


cd tmp
cat input.tdiag | ../dist/build/parse-tdiag/parse-tdiag > input_2.ast
cat input_2.ast | ../dist/build/tdiag2picture/tdiag2picture > input_3.pict


cat << "EOF" > input.tex
\documentclass[12pt, a4paper]{article}
\begin{document}
EOF

cat input_3.pict | ../dist/build/pp-picture/pp-picture >> input.tex

cat << "EOF" >> input.tex
\end{document}
EOF

pdflatex input.tex

cd ..
