MCPD project Group 8 --- sed
Lars Folkersma, Paul van Grol, Mark Leenen, Matej Milinkovic, Tom Smeding


   --- COMPILATION AND RUNNING ---

This folder is a Haskell Stack project. To compile and run, make sure you have
Stack (https://haskellstack.org) installed and working; then run:

    $ stack run

in the directory that contains this README. This runs the program and should
show brief usage information. As an example, consider the following invocation:

    $ stack run -- sed tests/subst.sed

This would run the subst.sed test file in our implementation, reading input from
stdin and writing output to stdout when the program is finished.


   --- FILES ---

All source files are in the directory containing this README. The file names
should make it obvious what is in the files.

The most interesting files to look at are the following:
- Dynamic.hs and Regex.hs, containing the dynamic semantics.
- Static.hs, containing the static semantics. This is a relatively
  straightforward implementation of the semantic rules.

Furthermore, the following source files are present:
- AST.hs contains the abstract syntax tree, which should not be important to
  read if one has read the program representation section in the report.
- Parser.hs contains a parser, written using parser combinators from the Parsec
  library; this is just utility code and is not important for the actual
  assignment.
- Main.hs contains the entry point to the program, showing how the interface is
  working.

There are a number of test files in the tests/ subdirectory. Note that all of
these files, with exception of subst.sed, are sourced from the GNU sed manual,
Examples section, and are thus not written by us.
