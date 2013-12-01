MCCO Project A


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

Running the programs:
$ cat ... | ./dist/build/PROG/PROG | ....

where PROG may be one of "parse-bib", "bib2html", "pp-html"

All options accept the "--sane" parameter. When given, show/read is used
instead of the cco ATerm parser/printer. This is required for unicode support
due to what seems to be a bug in the cco library.

To run everything with the same arguments:
$ cat .... | ./run-all.sh --sane


Running the tests:
$ make test

==========
DOCUMENTATION
==========

See Doc/Doc.pdf for the complete documentation. The features section of the pdf is copied here as requested on the cco website.

Features:


TODO

