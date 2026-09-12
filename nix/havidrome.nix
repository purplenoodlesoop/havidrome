{
  lib,
  pkgs,
  ...
}:
let
  inherit (pkgs) haskellPackages mpv-unwrapped;
  inherit (pkgs.haskell.lib.compose)
    disableOptimization
    enableDWARFDebugging
    dontStrip
    overrideCabal
    ;
  inherit (lib.fileset)
    toSource
    unions
    ;

  # nixpkgs-unstable has dropped x86_64-darwin: GHC's `meta.platforms` no
  # longer names it, so nothing written in Haskell can be built for it with
  # this input. The flake evaluates there and offers nothing of havidrome.
  buildable = pkgs.stdenv.hostPlatform.system != "x86_64-darwin";

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

  # The package as `cabal2nix` would have written it, by hand: the Haskell
  # dependencies arrive as arguments, which is what lets `overrideCabal` --
  # and so the wrapper, the debug build and the check built on it -- rewrite
  # the Cabal arguments afterwards.
  #
  # The three lists below and the `build-depends` of havidrome.cabal are the
  # same lists written twice, and nothing checks that they agree: whatever
  # goes into one goes into the other. The `havidrome` that the cabal's
  # executable and test suite depend on is this package's own library, built
  # here beside them, so it appears in neither list.
  package =
    {
      mkDerivation,
      QuickCheck,
      aeson,
      base,
      brick,
      bytestring,
      containers,
      crypton,
      directory,
      filepath,
      hspec,
      http-client,
      http-client-tls,
      http-types,
      mtl,
      network,
      optics-core,
      process,
      random,
      stm,
      temporary,
      text,
      transformers,
      unix,
      vector,
      vty,
    }:
    mkDerivation {
      pname = "havidrome";
      version = "1.0.0.0";
      src = source;

      isLibrary = true;
      isExecutable = true;

      libraryHaskellDepends = [
        aeson
        base
        brick
        bytestring
        crypton
        directory
        filepath
        http-client
        http-client-tls
        http-types
        mtl
        network
        optics-core
        process
        random
        stm
        text
        transformers
        unix
        vector
        vty
      ];

      executableHaskellDepends = [
        base
      ];

      testHaskellDepends = [
        QuickCheck
        aeson
        base
        brick
        bytestring
        containers
        directory
        filepath
        hspec
        http-client
        http-types
        stm
        temporary
        text
        transformers
        unix
        vector
        vty
      ];

      license = lib.licenses.mit;
      mainProgram = "havidrome";
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

  release = withPlayer (haskellPackages.callPackage package { });

  # The debug build is unoptimised and keeps its DWARF symbols, so a debugger
  # can follow it and `file` tells the two builds apart.
  debug = lib.pipe release [
    disableOptimization
    enableDWARFDebugging
    dontStrip
  ];
in
{
  flake = lib.optionalAttrs buildable {
    packages = {
      default = release;
      havidrome = release;
      havidrome-debug = debug;
    };

    apps.havidrome = release;
  };
}
