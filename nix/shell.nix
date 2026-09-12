{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (pkgs) haskellPackages mpv-unwrapped;

  # A GHC that already carries both packages' dependencies, so `cabal` in the
  # dev shell never reaches for Hackage. Neither havidrome package is among
  # them: the shell is where they are built from source.
  dependencies =
    package: builtins.filter (input: (input.pname or "") != "havidrome-core") package.getBuildInputs.haskellBuildInputs;

  ghc = haskellPackages.ghcWithPackages (
    _:
    dependencies config.flake.packages.havidrome-core
    ++ dependencies config.flake.packages.havidrome
  );
in
{
  # A shell to develop in exists where there is something to develop: on a
  # system havidrome is not built for, the compiler itself is unavailable.
  flake.shell = lib.optionals (config.flake.packages ? havidrome) [
    ghc
    haskellPackages.cabal-install
    # The same player the built executable carries, so `cabal run` and
    # `cabal test` in the shell make sound the same way.
    mpv-unwrapped
  ];
}
