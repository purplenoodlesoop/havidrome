{
  config,
  pkgs,
  ...
}:
let
  inherit (pkgs) haskellPackages mpv-unwrapped;

  # A GHC that already carries the package's dependencies, so `cabal` in the
  # dev shell never reaches for Hackage.
  ghc = haskellPackages.ghcWithPackages (
    _: config.flake.packages.havidrome.getBuildInputs.haskellBuildInputs
  );
in
{
  flake.shell = [
    ghc
    haskellPackages.cabal-install
    # The same player the built executable carries, so `cabal run` and
    # `cabal test` in the shell make sound the same way.
    mpv-unwrapped
  ];
}
