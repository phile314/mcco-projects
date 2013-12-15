MCCO Project B


Authors: Marco Vassena,    4110161
         Philipp Hausmann, 4003373


===========
BUILDING
===========

Prerequisites (other versions might work too):
 * GHC 7.4.2
 * uuagc 0.9.50
 * uuagc-cabal 1.0.5.0


Building:
$ make
or
$ cabal configure --enable-tests
$ cabal build

Running the programs:
$ cat <tdiag file> | ./dist/build/PROG/PROG | ....

where PROG may be one of "parse-tdiag", "tc-tdiag", "tdiag2picture", "pp-picture"


Render.sh can be used to run all sub-programs, create a latex document and run pdflatex on it.
All results will be stored in the folder where the script is located in the
subfolder "tmp". The result will be "tmp/input.pdf".
$ render.sh <tdiag fike>



Running the tests:
$ make test
or
$ cabal test

==========
Examples
==========

See the examples/.. directory. The file "examples/Description.txt" documents
which examples are well-formed and ill-formed.

==========
DOCUMENTATION
==========

See Doc/Doc.pdf for the complete documentation.
See also the haddock documentation.

Features:
  - Type checking, including using a bottom-type to collect as many errors as possible
  - Latex picture generation
  - Interpreting a program yields a new program
  - Variables
  - Precise and readable error messages

Unsupported features:
  - Only diagrams can be used as variable values, not strings
  - Depending on the diagram and the string lengths, parts may overlap
