# Asterix data processing tools

This project builds on top of
[asterix-specs](https://github.com/zoranbosnjak/asterix-specs) -
asterix specification definition files.

## Project structure

[/code-generator](code-generator) directory contains a source code generator.

Output of this program is a *library* source code for the target programming
language, for example: `asterix.py` is generated, for parsing, creating and
manipulating asterix binary data in `python`.

The structure of the generator is prepared such that a support many target
programming languages can be added, while sharing the same generic asterix
structure.

[/commet-py](comet-py) is a generic asterix data manipulation program.
See [README.md](comet-py/README.md) for more details.

[/tests](tests) directory contains unit tests.

