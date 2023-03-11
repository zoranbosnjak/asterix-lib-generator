# comet-py asterix tool

- asterix random sample generator
- asterix decoder

## Usage tips and tricks

### Limit number of generated samples

```bash
comet-py random | head -n 10
```

### Limit sample generation speed/rate

```bash
comet-py random | while read x; do echo "$x"; sleep 0.5; done
comet-py random | pv -qL 300
```

### Prepend/append some string to generated samples

```bash
comet-py random | awk '{print "0: "$1}'
```

### Send random data to UDP

Use bash one-liner,

```bash
comet-py random | while read x; do echo $x | xxd -r -p | socat -u stdin udp-sendto:127.0.0.1:59123; sleep 0.5; done
```

or create a script with arguments, for example

```bash
touch random-udp.sh
chmod 755 random-udp.sh
cat << EOF > random-udp.sh
#!/usr/bin/env bash
sleeptime=\$1
destination=\$2
comet-py random | while read x; do echo \$x | xxd -r -p | socat -u stdin udp-sendto:\${destination}; sleep \${sleeptime}; done
EOF
```

and run it

```bash
./random-udp.sh 0.3 127.0.0.1:59123
```

### Run self-test

Check if the tool can parse all valid samples. This test will run until
interrupted or until an error is detected.

```bash
comet-py random | comet-py dump --stop-on-error > /dev/null
```

## Development

```bash
nix-shell

# generate/refresh python library source code from defined specs
aspecsDir=../../asterix-specs/
SPECS=$(find ${aspecsDir}/specs/cat* | grep "\.ast")
ast-code-generator --language python ${SPECS} > asterix.py

# type check
mypy --strict asterix.py
mypy --strict comet-py

# type check with real-time feedback
./checkpy.sh asterix.py
./checkpy.sh comet-py asterix.py
```

