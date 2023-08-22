# Testing python code

From `code-generator` directory,
generate/refresh library source `asterix.py`.

```bash
nix-shell

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

