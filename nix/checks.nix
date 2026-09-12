{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (pkgs) mpv-unwrapped;
  inherit (pkgs.haskell.lib.compose)
    addTestToolDepends
    appendConfigureFlag
    doCheck
    ;
in
{
  # `nix flake check` builds the package with its test suite enabled, on every
  # system the package exists for. The tests that drive a real player need one
  # to drive, on a null audio output: no device, but no stand-in either.
  #
  # The gate is also where -Werror lives: a warning must fail the check
  # without making the player itself unbuildable, so it is never in the
  # `.cabal`. It goes in at configure time because cabal drops a
  # `--ghc-options=-Werror` as an option that changes no build artifact, and a
  # Nix build compiles every module from scratch, so no warning is skipped
  # over as already built.
  flake.output.checks = lib.optionalAttrs (config.flake.packages ? havidrome) {
    havidrome-test = lib.pipe config.flake.packages.havidrome [
      (addTestToolDepends [ mpv-unwrapped ])
      doCheck
      (appendConfigureFlag "--ghc-option=-Werror")
    ];
  };
}
