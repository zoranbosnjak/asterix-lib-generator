{ gitrev ? null
, sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs {}
, inShell ? null
}:

let
  cmd1 = ''
    mkdir $out
    echo 0 > $out/timestamp
  '';

  cmd2 = ''
    mkdir $out
    ${pkgs.git}/bin/git show -s --format=%ct ${gitrev} > $out/timestamp
  '';

  timestamp = pkgs.stdenv.mkDerivation {
    name = "timestamp";
    src = ./.;
    installPhase = if gitrev == null then cmd1 else cmd2;
  };

  drv = import ./website { inherit gitrev pkgs timestamp; inShell = false; };
  env = pkgs.stdenv.mkDerivation {
    name = "asterix-lib-generator-environment";
    buildInputs = [];
    shellHook = ''
      echo "Run nix-shell inside individual sub-directory!"
      exit 1
    '';
  };

in
  if inShell == false
    then drv
    else if pkgs.lib.inNixShell then env else drv

