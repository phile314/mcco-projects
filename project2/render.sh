#!/bin/bash

# does not work if there are symlinks etc.
# http://stackoverflow.com/questions/59895/can-a-bash-script-tell-what-directory-its-stored-in
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
BIN="${DIR}/dist/build"

set -o pipefail

mkdir -p "${DIR}/tmp"

rm "${DIR}/tmp/input.tdiag"


cat $1 >> "${DIR}/tmp/input.tdiag"

OLD_PWD=`pwd`
cd "${DIR}/tmp"
cat input.tdiag | "${BIN}/parse-tdiag/parse-tdiag" > input_2.ast
if [ $? -ne 0 ]; then
    echo "Parse error, aborting..."
    exit 2
fi

cat input_2.ast | "${BIN}/tc-tdiag/tc-tdiag"
if [ $? -ne 0 ]; then
    echo "Type checking failed, aborting..."
    exit 2
fi

# rendering should never fail, if the above two steps worked
cat input_2.ast | "${BIN}/tdiag2picture/tdiag2picture" > input_3.pict


cat << "EOF" > input.tex
\documentclass[10pt, a4paper]{article}
\begin{document}
\begin{footnotesize}
EOF

cat input_3.pict | "${BIN}/pp-picture/pp-picture" >> input.tex

cat << "EOF" >> input.tex
\end{footnotesize}
\end{document}
EOF

pdflatex input.tex

cd "${OLD_PWD}"
