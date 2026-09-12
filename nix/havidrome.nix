{
  lib,
  pkgs,
  ...
}:
let
  inherit (pkgs) haskellPackages mpv-unwrapped;
  inherit (pkgs.haskell.lib.compose)
    addTestToolDepends
    appendConfigureFlag
    disableOptimization
    enableDWARFDebugging
    dontStrip
    doCheck
    overrideCabal
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

  # The audio comes out of mpv, and the build is what supplies it: the
  # installed player carries its own on its PATH, so a machine with no mpv
  # installed still plays.
  withPlayer = overrideCabal (drv: {
    buildTools = (drv.buildTools or [ ]) ++ [ pkgs.makeBinaryWrapper ];
    postInstall = (drv.postInstall or "") + ''
      wrapProgram $out/bin/havidrome --prefix PATH : ${lib.makeBinPath [ mpv-unwrapped ]}
    '';
  });

  release = withPlayer (haskellPackages.callCabal2nix "havidrome" source { });

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
      # The same player the built executable carries, so `cabal run` and
      # `cabal test` in the shell make sound the same way.
      mpv-unwrapped
    ];

    # `nix flake check` builds the package with its test suite enabled. The
    # tests that drive a real player need one to drive, on a null audio
    # output: no device, but no stand-in either.
    #
    # The gate is also where -Werror lives: a warning must fail the check
    # without making the player itself unbuildable, so it is never in the
    # `.cabal`. It goes in at configure time because cabal drops a
    # `--ghc-options=-Werror` as an option that changes no build artifact,
    # and a Nix build compiles every module from scratch, so no warning is
    # skipped over as already built.
    output.checks.havidrome-test = lib.pipe release [
      (addTestToolDepends [ mpv-unwrapped ])
      doCheck
      (appendConfigureFlag "--ghc-option=-Werror")
    ];
  };
}
