{ gitrev ? null
, sources ? import ../nix/sources.nix
, pkgs ? import sources.nixpkgs {}
, inShell ? null
, timestamp ?  pkgs.stdenv.mkDerivation {name = "timestamp"; src = ./.; installPhase = ''
      mkdir $out
      echo 0 > $out/timestamp
    '';}
}:

let
  haskellPackages = pkgs.haskellPackages;

  codeGenerator = import ../code-generator/default.nix { inherit pkgs; inShell = false; };

  aspecsRef = builtins.fromJSON (builtins.readFile ../nix/aspecs.json);
  aspecsDir = pkgs.fetchgit {
    url = aspecsRef.url;
    rev = aspecsRef.rev;
    sha256 = aspecsRef.sha256;
  };

  site = haskellPackages.callCabal2nix "website" ./. { };

  reference = if gitrev == null
    then "unknown"
    else "${gitrev}";

  libPython = import ../support/Language/Python/lib.nix { inherit reference timestamp codeGenerator aspecsDir; };

  envVars = ''
    export ASTERIX_SPECS=${aspecsDir}
    export SPECS=$(find ${aspecsDir}/specs/cat* | grep "\.ast")
    export REFERENCE=${reference}
    export ASTERIX_SPECS_REVISION=${aspecsRef.rev}
    export TIMESTAMP=$(cat ${timestamp}/timestamp)
    export VERSION=$(${codeGenerator}/bin/ast-code-generator \
      --language test \
      --timestamp $TIMESTAMP \
      --reference $REFERENCE \
      $SPECS \
      --show-version)
  '';

  deps = [
    pkgs.git
    codeGenerator
  ];

  env = pkgs.stdenv.mkDerivation rec {
    name = "website-devel-environment";
    buildInputs = site.env.nativeBuildInputs ++ deps;
    shellHook = envVars;
  };

  drv = pkgs.stdenv.mkDerivation {
    name = "asterix-lib-generator-website";
    preBuild = envVars;
    src = ./.;
    buildInputs = deps;
    installPhase = ''
      mkdir -p $out
      echo ${reference} > $out/reference.txt
      cat ${timestamp}/timestamp > $out/timestamp.txt

      # python lib
      mkdir -p $out/lib/python
      cp ${libPython}/* $out/lib/python

      # website
      rm -f result
      rm -rf _site _cache
      ${site}/bin/site rebuild
      cp -a _site/* $out
    '';
  };

in
  if inShell == false
    then drv
    else if pkgs.lib.inNixShell then env else drv

