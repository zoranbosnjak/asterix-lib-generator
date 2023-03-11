# Testing python code

```bash
nix-shell

# generate/refresh python library source code from sample specs
specs=$(find ../../specs/ | grep "\.ast")
ast-code-generator --language python ${specs} > asterix.py

# type check
mypy --strict asterix.py
mypy --strict test_*.py

# run tests
pytest
```

## Development

```bash
nix-shell
./checkpy.sh asterix.py
./checkpy.sh some-test-file.py asterix.py
```

