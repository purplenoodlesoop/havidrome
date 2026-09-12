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
  # Each package is its own source tree, rooted at its own directory, so
  # neither is rebuilt when the other changes.
  coreSource = toSource {
    root = ../core;
    fileset = unions [
      ../core/havidrome-core.cabal
      ../core/src
      ../core/fixtures
      ../core/test
    ];
  };

  tuiSource = toSource {
    root = ../tui;
    fileset = unions [
      ../tui/havidrome-tui.cabal
      ../tui/src
      ../tui/app
      ../tui/test
    ];
  };

  # The packages as `cabal2nix` would have written them, by hand: the Haskell
  # dependencies arrive as arguments, which is what lets `overrideCabal` --
  # and so the wrapper, the debug build and the checks built on them --
  # rewrite the Cabal arguments afterwards.
  #
  # The lists below and the `build-depends` of the two `.cabal` files are the
  # same lists written twice, and nothing checks that they agree: whatever
  # goes into one goes into the other. A package's own library, which its
  # other components depend on, is built here beside them and appears in no
  # list.
  corePackage =
    {
      mkDerivation,
      aeson,
      base,
      bytestring,
      containers,
      crypton,
      hedgehog,
      http-types,
      text,
      transformers,
    }:
    mkDerivation {
      pname = "havidrome-core";
      version = "1.0.0.0";
      src = coreSource;

      isLibrary = true;

      # The fixtures are a sub-library of this package, so what they take is
      # among what the libraries take.
      libraryHaskellDepends = [
        aeson
        base
        bytestring
        containers
        crypton
        http-types
        text
      ];

      testHaskellDepends = [
        aeson
        base
        bytestring
        hedgehog
        text
        transformers
      ];

      license = lib.licenses.mit;
    };

  tuiPackage =
    {
      mkDerivation,
      base,
      brick,
      bytestring,
      directory,
      filepath,
      havidrome-core,
      hedgehog,
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
      time,
      transformers,
      unix,
      vector,
      vty,
    }:
    mkDerivation {
      pname = "havidrome-tui";
      version = "1.0.0.0";
      src = tuiSource;

      isLibrary = true;
      isExecutable = true;

      libraryHaskellDepends = [
        base
        brick
        bytestring
        directory
        filepath
        havidrome-core
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
        time
        transformers
        unix
        vector
        vty
      ];

      executableHaskellDepends = [
        base
      ];

      testHaskellDepends = [
        base
        brick
        bytestring
        directory
        filepath
        havidrome-core
        hedgehog
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

  core = haskellPackages.callPackage corePackage { };

  release = withPlayer (haskellPackages.callPackage tuiPackage { havidrome-core = core; });

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
      havidrome-core = core;
      havidrome-debug = debug;
    };

    apps.havidrome = release;
  };
}
