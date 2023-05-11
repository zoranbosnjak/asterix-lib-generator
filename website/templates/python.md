---
title: Asterix python library
---

# Asterix library for `python`

- LIBRARY VERSION: `$version$`
- LIBRARY GIT REVISION: `$reference$`
- ASTERIX SPECS GIT REVISION: `$astReference$`

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

### Immutable objects

All operation on asterix objects are *immutable*.

For example:

```python
from asterix import *

Spec = CAT_002_1_1
# create empty record
rec0 = Spec.make_record({})

# this operation does nothing (result is not stored)
rec0.set_item('000', 1)
assert rec0.get_item('000') is None

# store result to 'rec1'
rec1 = rec0.set_item('000', 1)
assert rec1.get_item('000') is not None

# use multiple updates in sequence
rec2 = rec0.set_item('000', 1).set_item('010', {'SAC': 1, 'SIC': 2})
assert rec2 == Spec.make_record({'000': 1, '010': {'SAC': 1, 'SIC': 2}})

# mutation can be simulated by replacing old object with the new one
# (using the same variable name)
rec0 = rec0.set_item('000', 1)
assert rec0.get_item('000') is not None
```

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

**Example**: Asterix filter, rewrite SAC/SIC code with random values (complete example).

```python
import time
import random
from asterix import *

# categories/editions of interest
Specs = {
    48: CAT_048_1_31,
    62: CAT_062_1_19,
    63: CAT_063_1_6,
    # ...
    }

def process_record(sac, sic, rec):
    """Process single record."""
    # One option is to use 'set_item' to insert '010' unconditionally
    # return rec.set_item('010', {'SAC': sac, 'SIC': sic})
    # ... or 'modify_item', where '010' is modified only if already present
    return rec.modify_item('010', lambda _old: {'SAC': sac, 'SIC': sic})

def process_datablock(sac, sic, raw_db):
    """Process single raw datablock."""
    cat = raw_db.category
    Spec = Specs.get(cat)
    if Spec is None:
        return raw_db
    # second level of parsing (records are valid)
    db = Spec.parse(raw_db)
    new_records = [process_record(sac, sic, rec) for rec in db.records]
    return Spec.make_datablock(new_records)

def rewrite_sac_sic(sac : int, sic : int, s : bytes) -> bytes:
    """Process datagram."""
    # first level of parsing (datablocks are valid)
    raw_datablocks = RawDatablock.parse(s)
    result = [process_datablock(sac, sic, db) for db in raw_datablocks]
    output = b''.join([db.unparse() for db in result])
    return output

def rx_bytes_from_the_network():
    """Dummy rx function (generate valid asterix datagram)."""
    time.sleep(1)
    Spec = CAT_048_1_31
    rec = Spec.make_record({'010': 0, '040': 0})
    db1 = Spec.make_datablock([rec, rec])
    db2 = Spec.make_datablock([rec, rec])
    return b''.join([db1.unparse(), db2.unparse()])

def tx_bytes_to_the_network(s_output):
    """Dummy tx function."""
    print(hexlify(s_output))

# main processing loop
while True:
    s_input = rx_bytes_from_the_network()
    new_sac = random.randint(0,127)
    new_sic = random.randint(128,255)
    try:
        s_output = rewrite_sac_sic(new_sac, new_sic, s_input)
        tx_bytes_to_the_network(s_output)
    except AsterixError as e:
        print('Asterix exception: ', e)
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
$$ python some-asterix-related-program.py
... results in runtime error (line number 3)
... after fixing the first problem and re-running,
... another runtime error is generated on line number 4

$$ mypy some-asterix-related-program.py
... detects both problems, without actually running the program
Found 2 errors in 1 file (checked 1 source file)
```

## Module classes and functions

### AsterixError

```python
class AsterixError(Exception):
    def __init__(self, msg : Optional[str]=None):
class AsterixOverflow(AsterixError):
```

### RawDatablock class

```python
class RawDatablock:
    @classmethod
    def parse(cls, s : bytes) -> List['RawDatablock']:
        """Parse the first level of asterix to the list of results."""

    def unparse(self) -> bytes:

    @property
    def category(self) -> int:

    @property
    def length(self) -> int:

    @property
    def raw_records(self) -> bytes:
```

### Bits class

```python
class Bits:
    """Bit string, a wrapper around bytes (bytes, offset, size)."""
    @classmethod
    def empty(cls) -> 'Bits':

    @classmethod
    def from_bytes(cls, val : bytes) -> 'Bits':

    @classmethod
    def from_uinteger(cls, raw : int, o : int, n : int) -> 'Bits':

    def __len__(self) -> int:

    def __iter__(self) -> Iterator[bool]:

    def __eq__(self, other : Any) -> bool:

    def __str__(self) -> str:

    def split_at(self, n : int) -> Tuple['Bits', 'Bits']:

    def take(self, x : int) -> 'Bits':

    def drop(self, x : int) -> 'Bits':

    def __add__(self, other : 'Bits') -> 'Bits':

    def to_bytes(self) -> bytes:

    def to_uinteger(self) -> int:

    @classmethod
    def join(cls, lst : List['Bits']) -> 'Bits':
```

### Variation class and subclasses

```python
class Variation:
    """Baseclass for all variations."""
    def __init__(self, val : Bits):

    def unparse_bits(self) -> Bits:

    def __eq__(self, other : object) -> bool:

    def to_uinteger(self) -> int:

class Element(Variation):
    bit_offset8 : int
    bit_size : int
    string_type : StringType
    quantity : Quantity

    @classmethod
    def parse_bits(cls, s : Bits) -> Any:

    def _from_raw(self, raw : Raw) -> Bits:

    def _from_string(self, s : str) -> Bits:

    def _from_float(self, val : float) -> Bits:

    def _to_string(self) -> str:

    def _to_quantity(self) -> float:

class Group(Variation):
    subitems_list : List[Union[Spare, Tuple[ItemName, Any]]]
    subitems_dict : Dict[ItemName, Tuple[str, Any, int, int]]

    @classmethod
    def parse_bits(cls, s : Bits) -> Any:

    def __init__(self, val : Bits, items : Dict[ItemName, Any]):

    def _from_items(self, args : Any) -> Tuple[Bits, Dict[ItemName, Element]]:

    def _from_raw(self, raw : Raw) -> Tuple[Bits, Dict[ItemName, Element]]:

    def _get_item(self, name : Any) -> Any:

    def _set_item(self, name : Any, val : Any) -> Any:

    def _modify_item(self, name : Any, f : Any) -> Any:

class Extended(Variation):
    no_trailing_fx : bool # See [ref:extended-no-trailing-fx].
    prim_bit_size : int
    ext_bit_size : int
    groups_bit_sizes : List[int]
    subitems_list : List[List[Union[Spare, Tuple[ItemName, Any]]]]
    subitems_dict : Dict[ItemName, Tuple[str, Any, int, int]]

    @classmethod
    def parse_bits(cls, s : Bits) -> Any:

    def __init__(self, val : Bits, items : Dict[ItemName, Element]):

    def _from_tuple_int(self, val : Any) -> Tuple[Bits, Dict[ItemName, Element]]:

    def _from_dict(self, n : int, arg : Any) -> Tuple[Bits, Dict[ItemName, Element]]:

    def _get_item(self, name : Any) -> Any:

    def _set_item(self, name : Any, val : Any) -> Any:

    def _modify_item(self, name : Any, f : Any) -> Any:

class Repetitive(Variation):
    rep_byte_size : int
    variation_bit_size : int
    variation_type : Any

    @classmethod
    def parse_bits(cls, s : Bits) -> Any:

    def __init__(self, val : Bits, items : List[Variation]):

    def _from_list(self, lst : List[Any]) -> Tuple[Bits, Any]:

    def __len__(self) -> Any:

    def __iter__(self) -> Any:

    def __getitem__(self, ix : int) -> Any:

    def _append_item(self, arg : Any) -> Any:

    def _prepend_item(self, arg : Any) -> Any:

class Explicit(Variation):

    @classmethod
    def parse_bits(cls, s : Bits) -> Any:

    def __init__(self, val : Bits, raw : bytes):

    def _from_bytes(self, arg : bytes) -> Tuple[Bits, bytes]:

    @property
    def raw(self) -> bytes:

class Compound(Variation):
    fspec_fx : bool
    fspec_max_bytes : int
    subitems_list : List[Optional[Tuple[ItemName, Any]]]
    subitems_dict : Dict[ItemName, Tuple[Any, int]]

    @classmethod
    def parse_bits(cls, s : Bits) -> Any:

    def __init__(self, val : Bits = Bits.empty(), items : Dict[ItemName, Variation] = {}) -> None:

    def __bool__(self) -> bool:

    def _set_item(self, name : ItemName, val : Any) -> Any:

    def _update(self, args : Any) -> Any:

    def _del_item(self, name : ItemName) -> Any:

    def _get_item(self, name : ItemName) -> Any:

    def _modify_item(self, name : Any, f : Any) -> Any:
```

### Datablock class

```python
T = TypeVar('T')
class Datablock(Generic[T]):
    """Correctly constructed/parsed datablock."""
    def __init__(self, cat : int, lst : Union[T, List[T]], val : Optional[bytes] = None):

    def unparse(self) -> bytes:

    def __eq__(self, other : Any) -> bool:

    @property
    def records(self) -> List[T]:
```

### AsterixSpec class and subclasses

```python
class AsterixSpec:
    """Asterix base class."""
    cat : int

class Basic(AsterixSpec):
    variation : Any
    uaps : Any
    uap_selector_item : Any
    uap_selector_table : Any

    @classmethod
    def _parse(cls, raw_db : RawDatablock, uap : Optional[str] = None) -> Any:

    @classmethod
    def _is_valid(cls, rec : Any) -> bool:

class Expansion(AsterixSpec):
    variation : Any
```

### Generated subclasses

#### Element

```python
class Variation_XY(Element):
    variation = 'Element'
    bit_offset8 = ... (int)
    bit_size = ... (int)

    # if content == table
    table = {
        0: 'val0',
        1: 'val1',
        ...
    }

    # if content == string
    string_type = StringAscii() | StringICAO() | StringOctal()

    # if content == quantity
    quantity = Quantity(...)
```

#### Group

```python
class Variation_XY(Group):
    variation = 'Group'
    bit_size = 16
    subitems_list = [
        ('NAME1', Variation_1),
        ('NAME2', Variation_2),
        ...
    ]

    # name: (title, cls, group_offset, bit_size)
    subitems_dict = {
        'NAME1': ('Title 1', Variation_1, 0, 8),
        'NAME2': ('Title 2', Variation_2, 0, 8),
        ...
    }

    @classmethod
    def spec(cls, key : Union[Literal['NAME1'], Literal['NAME2']]) -> Union[Type['Variation_1'], Type['Variation_2']]:
        """Get spec of subitem."""

    def __init__(self, arg : Variation_XY_Arg) -> None:
        if isinstance(arg, tuple):
        if isinstance(arg, dict):
        if isinstance(arg, Raw):
        assert_never(arg)

    def get_item(self, name : Union[Literal['NAME1'], Literal['NAME2']]) -> Any:

    def set_item(self, name : Any, val : Any) -> Any:

    def modify_item(self, name : Any, f : Any) -> Any:
```

Group item manipulation example:

```python
from asterix import *

item_spec = CAT_048_1_31.spec('010')
item1 = item_spec({'SAC': 1, 'SIC': 2})         # create group item
assert item1.get_item('SAC').to_uinteger() == 1 # get_item
item2 = item1.set_item('SAC', 2)                # set item
assert item2.get_item('SAC').to_uinteger() == 2 # check
item3 = item1.modify_item('SAC', lambda x: x.to_uinteger()+1) # modify item
assert item2 == item3                           # check
```

#### Extended

```python
class Variation_XY(Extended):
    variation = 'Extended'
    no_trailing_fx = False | True
    prim_bit_size = (int)
    ext_bit_size = (int)
    groups_bit_sizes = [
        (int),
        (int),
        ...
    ]

    subitems_list = [
        [   # primary part
            ('NAME1', Variation_1),
        ],
        [   # extension
            ('NAME2', Variation_2),
            ('NAME3', Variation_3),
        ],
        [   # extension
            Spare((int), (int)),
            ('NAME4', Variation_4),
        ],
    ]

    # name: (title, cls, group_offset, bit_size)
    subitems_dict = {
        'NAME1': ('Title 1', Variation_1, 0, 7),
        'NAME2': ('Title 2', Variation_2, 0, 7),
        'NAME3': ('Title 3', Variation_3, 0, 7),
    }

    @classmethod
    def spec(cls, key : Union[Literal['NAME1'], Literal['NAME2']]) -> Union[Type['Variation_1'], Type['Variation_2']]:
        """Get spec of subitem."""

    def __init__(self, arg : Variation_13_Arg) -> None:
        if isinstance(arg, int):
        if isinstance(arg, tuple):
        if isinstance(arg, dict):
        assert_never(arg)

    def get_item(self, name : Union[Literal['NAME1'], Literal['NAME2'], Literal['NAME3']]) -> Any:

    def set_item(self, name : Any, val : Any) -> Any:

    def modify_item(self, name : Any, f : Any) -> Any:
```

Extended item manipulation example:

```python
from asterix import *

item_spec = CAT_048_1_31.spec('020')

# create extended item (primary + 1 extension, all zero)
item1 = item_spec((0,0))

# get_item
assert item1.get_item('TYP').to_uinteger() == 0
assert item1.get_item('TST').to_uinteger() == 0
assert item1.get_item('ADSB') is None

# set_item works on primary part
item2 = item1.set_item('TYP', 1)
assert item2.get_item('TYP').to_uinteger() == 1

# set_item does not work on extension
# item3 = item1.set_item('TST', 1)  <- this is not possible

# modify_item works on any subitem
item3 = item1.modify_item('TST', lambda x: 1)   # modify with const(1)
assert item3.get_item('TST').to_uinteger() == 1

# if subitem is not present, modify_item has no effect
item4 = item1.modify_item('ADSB', lambda x: 1)
assert item4.get_item('ADSB') is None
```

#### Repetitive

```python
class Variation_XY(Repetitive):
    variation = 'Repetitive'
    rep_byte_size = (int)
    variation_bit_size = (int)
    variation_type = Variation_1

    @classmethod
    def spec(cls) -> Type[Variation_1]:
        """Get spec of subitem."""

    def __init__(self, arg : List[Union[Variation_1, Variation_1_Arg]]) -> None:
        if isinstance(arg, tuple):
        if isinstance(arg, list):
        assert_never(arg)

    def append_item(self, arg : Union[Variation_1, Variation_1_Arg]) -> 'Variation_XY':

    def prepend_item(self, arg : Union[Variation_1, Variation_1_Arg]) -> 'Variation_XY':
```

Repetitive item manipulation example:

```python
from asterix import *

item_spec = CAT_048_1_31.spec('250')

# create repetitive item
item1 = item_spec([0,1])
assert len(item1) == 2
assert item1[0].to_uinteger() == 0
assert item1[1].to_uinteger() == 1

item2 = item1.append_item(5)
assert item2 == item_spec([0,1,5])

item3 = item1.prepend_item(6)
assert item3 == item_spec([6,0,1])
```

#### Explicit

```python
class Variation_XY(Explicit):
    variation = 'Explicit'
    def __init__(self, arg : bytes) -> None:
        if isinstance(arg, tuple):
        if isinstance(arg, bytes):
        assert_never(arg)
```

Explicit item manipulation example:

```python
from asterix import *

item_spec = CAT_048_1_31.spec('RE')

val = 0x01020304
bs = val.to_bytes(4, 'big')
i = item_spec(bs)   # create explicit item from bytes
assert i.raw == bs  # check
```

#### Compound

```python
class Variation_XY(Compound):
    variation = 'Compound'
    fspec_fx = False | True
    fspec_max_bytes = (int)

    subitems_list = [
        ('NAME1', Variation_1),
        None,
        ('NAME2', Variation_2),
        ('NAME3', Variation_3),
        ...
    ]

    # name: (cls, fspec)
    subitems_dict = {
        'NAME1': (Variation_1, 0x8000),
        'NAME2': (Variation_2, 0x2000),
        'NAME3': (Variation_3, 0x0180),
        ...
    }

    @classmethod
    def spec(cls, key : Union[Literal['NAME1'], Literal['NAME2']]) -> Union[Type['Variation_1'], Type['Variation_2']]:
        """Get spec of subitem."""

    def __init__(self, arg : Optional[Variation_XY_Arg] = None) -> None:

    def set_item(self, name : Any, val : Any) -> Any:

    def del_item(self, name : Any) -> Any:

    def get_item(self, name : Any) -> Any:

    def modify_item(self, name : Any, f : Any) -> Any:
```

Compound item manipulation example:

```python
from asterix import *

item_spec = CAT_048_1_31.spec('120')
item = item_spec()                              # create empty compound item

assert item.get_item('CAL') is None                             # get_item
item1 = item.set_item('CAL', 0)                                 # set_item
assert item1.get_item('CAL').to_uinteger() == 0
assert item1.del_item('CAL').get_item('CAL') is None            # del_item

def succ(x : Any) -> Any:
    return x.to_uinteger() + 1

# modify_item (called on 2 items)
item2 = item1.modify_item('CAL', succ).modify_item('RDS', succ)

assert item2.get_item('CAL').to_uinteger() == 1     # updated item
assert item2.get_item('RDS') is None                # not present
```

#### Basic spec

```python
class CAT_(cat)_(edMajor)_(edMinor)(Basic):
    cat = (int)
    variation = Variation_XY
    spec = variation.spec
    parse_bits = variation.parse_bits
    unparse_bits = variation.unparse_bits

    # if multiple UAPs are present
    uaps = {
        'uap0': Variation_0,
        'uap1': Variation_1,
        ...
    }

    uap_selector_item = None | ["ITEM", "SUBITEM"]

    uap_selector_table = None | {
        0: 'uap0',
        1: 'uap1',
        ...
    }

    @classmethod
    def make_record(cls, val : Variation_XY_Arg) -> Variation_XY:

    @classmethod
    def make_datablock(cls, val : Union[Variation_XY, List[Variation_XY]]) -> Datablock[Variation_XY]:

    @classmethod
    def parse(cls, val : RawDatablock) -> Datablock[Variation_XY]:
```

#### Expansion spec

```python
class REF_000_1_0(Expansion):
    cat = (int)
    variation = Variation_XY
    spec = variation.spec
    parse_bits = variation.parse_bits
    unparse_bits = variation.unparse_bits

    @classmethod
    def make_extended(cls, val : Variation_XY_Arg) -> Variation_XY:

    @classmethod
    def parse(cls, val : bytes) -> Variation_XY:
```

#### Generated manifest

Example:

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
