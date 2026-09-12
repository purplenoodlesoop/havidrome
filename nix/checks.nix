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

  # The gate is where -Werror lives: a warning must fail the check without
  # making the player itself unbuildable, so it is never in a `.cabal`. It
  # goes in at configure time because cabal drops a `--ghc-options=-Werror`
  # as an option that changes no build artifact, and a Nix build compiles
  # every module from scratch, so no warning is skipped over as already
  # built.
  tested = lib.flip lib.pipe [
    doCheck
    (appendConfigureFlag "--ghc-option=-Werror")
  ];
in
{
  # `nix flake check` builds both packages with their test suites enabled, on
  # every system they exist for. The shell's tests drive a real player on a
  # null audio output: no device, but no stand-in either. The core's drive
  # nothing, which is the point of it.
  flake.output.checks = lib.optionalAttrs (config.flake.packages ? havidrome) {
    havidrome-core-test = tested config.flake.packages.havidrome-core;

    havidrome-tui-test = tested (addTestToolDepends [ mpv-unwrapped ] config.flake.packages.havidrome);
  };
}
