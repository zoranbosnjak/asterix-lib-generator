{ reference
, genTimestamp
, codeGenerator
, aspecsDir
}:

let
  lang = "python";
  filename = "asterix.py";
  name = "asterix-library-${lang}";
  sources = import ../../../nix/sources.nix;
  packages = import sources.nixpkgs {};

  deps = [
  ];

in with packages; runCommand name {
  propagatedBuildInputs = deps;
  base = ../../../.;
  src = ./.;
  }
  ''
    export SPECS=$(find ${aspecsDir}/specs/cat* | grep "\.ast")
    export REFERENCE=${reference}
    export TIMESTAMP=$(${genTimestamp})
    mkdir $out

    echo timestamp $TIMESTAMP
    echo reference $REFERENCE

    ${codeGenerator}/bin/ast-code-generator \
      --language ${lang} \
      --timestamp $TIMESTAMP \
      --reference $REFERENCE \
      $SPECS \
      > $out/${filename}

    mkdir -p $out/asterix-lib/src
    cp $base/LICENSE $src/pyproject.toml $out/asterix-lib
    echo "# asterix library for ${lang}" > $out/asterix-lib/README.md
    touch $out/asterix-lib/src/__init__.py
    cp $out/${filename} $out/asterix-lib/src/
    cd $out
    tar -czf asterix-lib.tgz asterix-lib
    rm -rf asterix-lib
  ''

