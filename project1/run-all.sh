#! /bin/sh
# Runs all the commands.
# Supported flag 
  # --sane : used for dealing utf8 encodings

EXEC=dist/build
PARSE_BIB=$EXEC/parse-bib/parse-bib
BIB2HTML=$EXEC/bib2html/bib2html
PP_HTML=$EXEC/pp-html/pp-html

$PARSE_BIB $1 | $BIB2HTML $1 | $PP_HTML $1
