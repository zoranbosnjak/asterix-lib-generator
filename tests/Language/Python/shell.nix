{ sources ? import ../../../nix/sources.nix
, pkgs ? import sources.nixpkgs {}
}:

let
  codeGenerator = import ../../../code-generator/default.nix {inShell = false;};
  customPython = pkgs.python311.buildEnv.override {
    extraLibs = [
      pkgs.python311Packages.mypy
      pkgs.python311Packages.pytest
      pkgs.python311Packages.hypothesis
    ];
  };

in pkgs.stdenv.mkDerivation rec {
  name = "python-environment";

  buildInputs = [
    customPython
    codeGenerator
  ];

  shellHook = ''
    export PYTHONPATH=$(pwd):$(pwd)/specs:$PYTHONPATH
  '';
}

