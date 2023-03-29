{ gitrev ? null
, sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs {}
, inShell ? null
}:

let
  drv = import ./website { inherit gitrev; inherit pkgs; inShell = false; };
  env = pkgs.stdenv.mkDerivation {
    name = "comet-environment";
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

