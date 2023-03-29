# Source code generator for processing asterix data

## Building and running

```bash
nix-build
./result/bin/ast-code-generator -h
```

## Development environment

```bash
nix-shell

# check defined and used references
tagref

# run 'ghcid'
ghcid "--command=ghci -Wall -iother -ilib -isrc -iapp app/Asterix-code-generator.hs"

# run program, show usage
runhaskell $EXTENSIONS -iother -ilib -iapp ./app/Asterix-code-generator.hs --help

# generate python code for test run
runhaskell $EXTENSIONS -iother -ilib -iapp ./app/Asterix-code-generator.hs \
    --language python ../tests/specs/*ast | tee ../tests/Language/Python/asterix.py

# generate python code with all defined specs
runhaskell $EXTENSIONS -iother -ilib -iapp ./app/Asterix-code-generator.hs \
    --language python $SPECS

exit
```

