{ sources ? import ../nix/sources.nix
, pkgs ? import sources.nixpkgs {}
, gitrev ? "devel"
, inShell ? null
, withHoogle ? false
, static ? false
}:

let

  deps = with pkgs; [
    # package1
    # package2
    # ...
  ];

  aspecsRef = builtins.fromJSON (builtins.readFile ../nix/aspecs.json);
  aspecsDir = pkgs.fetchgit {
    url = aspecsRef.url;
    rev = aspecsRef.rev;
    sha256 = aspecsRef.sha256;
  };

  haskellPackages1 = if static == true
    then pkgs.pkgsStatic.haskellPackages
    else pkgs.haskellPackages;

  haskellPackages = haskellPackages1.override {
    overrides = haskellPackagesNew: haskellPackagesOld: rec {
      aspecs = haskellPackagesNew.callPackage "${aspecsDir}/tools" {packages=pkgs; inShell=false;};
    };
  };

  buildExports = ''
      export LC_ALL=C.UTF-8
      export GHC_BASE=$(which ghc | cut -d '/' -f-4)
      export EXTENSIONS=$(cat .ghci | grep ":set -X" | awk '{print $2}' | xargs)
      export GIT_REV=${gitrev}
      export ASTERIX_SPECS=${aspecsDir}
      export SPECS=$(find ${aspecsDir}/specs/cat* | grep "\.ast")
    '';

  drv1 = haskellPackages.callCabal2nix "generator" ./. { };

  drv = drv1.overrideDerivation (oldAttrs: {
    src = builtins.filterSource
      (path: type:
        (type != "directory" || baseNameOf path != ".git")
        && (type != "symlink" || baseNameOf path != "result"))
        ./.;
    preBuild = buildExports;
  });

  env = haskellPackages.shellFor {
    packages = p: with p; [
      drv
    ];

    buildInputs = with haskellPackages; [
      niv
      pkgs.cacert # needed for niv
      pkgs.nix    # needed for niv
      cabal-install
      pkgs.ghcid
      pkgs.cabal2nix
      pkgs.which
      pkgs.tagref
    ];

    withHoogle = withHoogle;

    shellHook = buildExports;
  };

in
  if inShell == false
    then drv
    else if pkgs.lib.inNixShell then env else drv

