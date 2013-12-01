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
$ cat <bibtex file> | ./dist/build/PROG/PROG | .....

where PROG may be one of "parse-bib", "bib2html", "pp-html"

All progams accept the "--sane" parameter. When given, show/read is used
instead of the cco ATerm parser/printer. This is required for unicode support
due to what seems to be a bug in the cco library.

To run everything with the same arguments:
$ cat .... | ./run-all.sh [--sane]


Running the tests:
$ make test

==========
Examples
==========

See the examples/.. directory.

The example "mcco-example_simplified.bibtex" can be processed without the "--sane" option. Most 
if not all other examples require the "--sane" option.

==========
DOCUMENTATION
==========

See Doc/Doc.pdf for the complete documentation.
See also the haddock documentation.

The features section of the pdf is copied here as requested on the course website.

Features:
  - Error messages generation
  - Warning messages generation for conflicting and unknown fields, unknown entry types, which are then ignored
  - Accents in field (latex special syntax for unicode character such as \{"o})
  - Generation of convenient reference names ([LO93])
  - Formatting rules for names
  - Unicode support

Unsupported features:
  - Brackets around values
  - Warnings for duplicate bibtex entry keys
  - Unique reference names
  - The order used for the fields in some bibtex entries may be wrong
  - Some escaping rules are missing.
