---
title: Asterix python library
---

# Asterix library for `python`

Features:

- parse asterix data to datablocks and records (asterix receiver)
- create asterix data from values (asterix transmiter)
- manipulate/update asterix data (asterix filter)

## Installation

This library is a *single file* python module. To install it, use any of the
following methods:

### Method 1 - copy library file

Download and copy [this file](lib/python/asterix.py) alongside your project
sources or to some location where `python` can find it.

```bash
# copy file to project sources
cp ~/Downloads/asterix.py path/to/my-asterix-python-project/src
# or
cd path/to/my-asterix-python-project/src
wget https://zoranbosnjak.github.io/asterix-lib-generator/lib/python/asterix.py

# or check default python path and copy to the appropriate location
python3 -c "import sys; print('\n'.join(sys.path))"
```

### Method 2 - install/update package with `pip`

The library is also available as a [python package](lib/python/asterix-lib.tgz).
Use `pip` to install or update:

```bash
pip install https://zoranbosnjak.github.io/asterix-lib-generator/lib/python/asterix-lib.tgz
```

## Tutorial

Check library installation.

```bash
python3 -c "import asterix; print(asterix.manifest); print(asterix.VERSION)"
```

### Import

This tutorial assumes importing complete `asterix` module into the current
namespace. In practice however only the required objects could be imported
or the module might be imported to a dedicated namespace.

```python
from asterix import *
```

### Error handling

Some operation (eg. parsing) can fail on unexpected input with `AsterixError`.
The way to handle it is to use `try, except`, for example:

```python
try:
    raw_datablocks = RawDatablock.parse(input_data)
    process_datablocks(raw_datablocks)
except AsterixError as e:
    print('Exception: ', e)
```

For clarity, the error handling part is skipped in the rest of this tutorial.

### Datagram

Datagram is a raw binary data as received for example from UDP socket.
This is represented with `bytes` data type in python.

### Raw Datablock

Raw datablock is asterix datablock in the form `cat|length|data` with the
correct byte size. A datagram can contain multiple datablocks.

This is represented in python with `class RawDatablock`.

In some cases it might be sufficient to work with raw datablocks, for example
in the case of asterix category filtering. In this case, it is not required
to fully parse asterix records.

**Example**: Category filter, drop datablocks if category == 1

```python
def receive_from_udp():         # UDP rx text function
    return unhexlify(''.join([
        '01000401', # cat1 datablock
        '02000402', # cat2 datablock
        ]))

def send_to_udp(s):             # UDP tx test function
    print(hexlify(s))

input_data = receive_from_udp()
raw_datablocks = RawDatablock.parse(input_data) # can fail on wrong input
valid_datablocks = [db.unparse() for db in raw_datablocks if db.category != 1]
output_data = b''.join(valid_datablocks)
send_to_udp(output_data)
```

### Datablock, Record

Datablock (represented as `class Datablock`) is a higher level, where we
have a guarantee that all containing records are semantically correct
(asterix is fully parsed or correctly constructed).

Datablock/Record is required to work with asterix items and subitems.

**Example**: Create 2 records and combine them to a single datablock

```python
Spec = CAT_002_1_1  # use cat002, edition 1.1

rec1 = Spec.make_record({
    '000': 1,
    '010': {'SAC': 1, 'SIC': 2},
    })

rec2 = Spec.make_record({
    '000': 2,
    '010': {'SAC': 1, 'SIC': 2},
    })

db = Spec.make_datablock([rec1, rec2])
s = db.unparse() # ready to send over the network
print(hexlify(s))
```

**Example**: Parse datagram (from the example above) and extract message type
from each record

```python
# s = ... use data from the example above
raw_datablocks = RawDatablock.parse(s)      # can fail on wrong input
for raw_datablock in raw_datablocks:
    datablock = Spec.parse(raw_datablock)   # can fail on wrong input
    for record in datablock.records:
        message_type = record.get_item('000') # returns None if the item is not present
        print('{}: {}'.format(message_type.to_uinteger(), message_type.table_value))
```

#### Reserved expansion fields

- TODO: Parsing expansion field
- TODO: Constructing expansion field

#### Multiple UAP-s

TODO... see tests

### Library manifest

This library defines a `manifest` structure in the form:

```python
manifest = {
    'CATS': {
        1: {
            '1.2': CAT_001_1_2,
            '1.3': CAT_001_1_3,
            '1.4': CAT_001_1_4,
        },
        2: {
            '1.0': CAT_002_1_0,
            '1.1': CAT_002_1_1,
        #...
```

This structure can be used to extract *latest* editions for each defined
category, for example:

```python
def to_edition(ed):
    """Convert edition string to a tuple, for example "1.2" -> (1,2)"""
    a,b = ed.split('.')
    return (int(a), int(b))

def get_latest_edition(lst):
    return sorted(lst, key=lambda pair: to_edition(pair[0]), reverse=True)[0]

Specs = {}  # will be populated with latest editions

for cat in range(1,256):
    editions = manifest['CATS'].get(cat)
    if editions is None:
        continue
    latest = get_latest_edition(editions.items())
    ed, cls = latest
    Specs[cat] = cls
```

Alternatively, a prefered way is to be explicit about each edition,
for example:

```python
Specs = {
    48: CAT_048_1_31,
    62: CAT_062_1_19,
    63: CAT_063_1_6,
    # ...
    }
```

### Generic asterix processing

*Generic processing* in this context means working with asterix data where
the subitem names and types are determined at runtime. That is: the explicit
subitem names are never mentioned in the code.

This is in contrast to *application specific processing*, where we are
explicit about subitems, for example ["010", "SAC"].

**Example**: Show raw content of all toplevel items of each record

```python
# Specs = ... category/edition selection
# s = ... some input bytes
for raw_datablock in RawDatablock.parse(s):
    Spec = Specs.get(raw_datablock.category)
    if Spec is None:
        print('unsupported category')
        continue
    datablock = Spec.parse(raw_datablock)
    cat = raw_datablock.category
    for record in datablock.records:
        cls = record.__class__
        for topitem in cls.subitems_list:
            if topitem is None:
                continue
            name, subclass = topitem
            value = record.get_item(name)
            if value is None:
                continue
            value = hex(value.to_uinteger())
            print('cat{}, item {}, value {}'.format(cat, name, value))
            # depending on the application, we might want to display
            # deep subitems, which is possible by examining each toplevel item
```

**Example**: Generate dummy single record datablock with all fixed items set to zero

```python
# we could even randomly select a category/edition from the 'manifest',
# but for simplicity just use a particular one
Spec = CAT_062_1_19

rec = Spec.make_record({})
for topitem in Spec.variation.subitems_list:
    if topitem is None:
        continue
    name, subclass = topitem
    if issubclass(subclass, Element):
        rec = rec.set_item(name, 0)
    elif issubclass(subclass, Group):
        rec = rec.set_item(name, 0)
    elif issubclass(subclass, Extended):
        pass # skip for this test
    elif issubclass(subclass, Repetitive):
        pass # skip for this test
    elif issubclass(subclass, Explicit):
        pass # skip for this test
    elif issubclass(subclass, Compound):
        pass # skip for this test
    else:
        raise Exception('unexpected subclass')

s = Spec.make_datablock(rec).unparse()
print(hexlify(s))
```

## Using `mypy` static code checker

**Note**: `mypy` version `0.991` or above is required for this library.

[mypy](https://www.mypy-lang.org/) is a static type checker for Python.
It is recommended to use the tool on asterix application code, to identify
some problems which would otherwise result in runtime errors.

For example, the following program (`some-asterix-related-program.py`)
contains 2 bugs (misspelled item name, `SA` instead of `SAC`).

```python
Spec = CAT_008_1_3
rec = Spec.make_record({'010': {'SA': 1, 'SIC': 2}})
print(rec.get_item('010').get_item('SA').to_uinteger())
```

```
$ python some-asterix-related-program.py
... results in runtime error (line number 3)
... after fixing the first problem and re-running,
... another runtime error is generated on line number 4

$ mypy some-asterix-related-program.py
... detects both problems, without actually running the program
Found 2 errors in 1 file (checked 1 source file)
```

## Reference manual

TODO...

