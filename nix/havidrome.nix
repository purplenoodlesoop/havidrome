{
  lib,
  pkgs,
  ...
}:
let
  inherit (pkgs) haskellPackages;
  inherit (pkgs.haskell.lib.compose)
    disableOptimization
    enableDWARFDebugging
    dontStrip
    doCheck
    ;
  inherit (lib.fileset)
    toSource
    unions
    ;

  # Only what the compiler reads, so that touching a note does not rebuild.
  source = toSource {
    root = ../.;
    fileset = unions [
      ../havidrome.cabal
      ../src
      ../app
      ../test
    ];
  };

  release = haskellPackages.callCabal2nix "havidrome" source { };

  # The debug build is unoptimised and keeps its DWARF symbols, so a debugger
  # can follow it and `file` tells the two builds apart.
  debug = lib.pipe release [
    disableOptimization
    enableDWARFDebugging
    dontStrip
  ];

  # A GHC that already carries the package's dependencies, so `cabal` in the
  # dev shell never reaches for Hackage.
  ghc = haskellPackages.ghcWithPackages (_: release.getBuildInputs.haskellBuildInputs);
in
{
  flake = {
    packages = {
      default = release;
      havidrome = release;
      havidrome-debug = debug;
    };

    apps.havidrome = release;

    shell = [
      ghc
      haskellPackages.cabal-install
    ];

    # `nix flake check` builds the package with its test suite enabled.
    output.checks.havidrome-test = doCheck release;
  };
}
