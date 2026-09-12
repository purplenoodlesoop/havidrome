{
  ai-haskell-linter,
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (ai-haskell-linter.packages.${pkgs.stdenv.hostPlatform.system}) hlint hlint-config;
  inherit (lib.fileset) toSource unions;

  # A system havidrome is not built for has no Haskell in it to lint, and
  # importing nixpkgs there throws before hlint can be reached at all.
  lintable = config.flake.packages ? havidrome;

  root = ../.;

  # Every directory either package compiles, the core's fixtures included.
  # Written out rather than searched for, so nothing outside them can make
  # the gate run again -- or quietly go unlinted.
  directories = [
    "core/src"
    "core/fixtures"
    "core/test"
    "tui/src"
    "tui/app"
    "tui/test"
  ];

  # The relaxations of the shared ruleset, named after it and passed after
  # it, so that a rule let off in one module is let off nowhere else.
  local = ".hlint-local.yaml";

  source = toSource {
    inherit root;
    fileset = unions ([ (root + "/${local}") ] ++ map (directory: root + "/${directory}") directories);
  };

  # hlint parses with its own defaults, not the project's, so it is told the
  # language the code is written in. This list is the `default-language` and
  # `default-extensions` of both packages' `common language` stanza written a
  # second time; whatever changes in one changes here. Order matters --
  # GHC2021 does not include LambdaCase, and a later -X wins.
  language = [
    "-XGHC2021"
    "-XDerivingStrategies"
    "-XDerivingVia"
    "-XDeriveAnyClass"
    "-XGeneralizedNewtypeDeriving"
    "-XLambdaCase"
    "-XMultiWayIf"
    "-XBlockArguments"
    "-XViewPatterns"
    "-XPatternSynonyms"
    "-XQuasiQuotes"
    "-XApplicativeDo"
    "-XOverloadedStrings"
    "-XOverloadedLabels"
    "-XOverloadedRecordDot"
    "-XDuplicateRecordFields"
    "-XNoFieldSelectors"
    "-XRecordWildCards"
    "-XNamedFieldPuns"
    "-XTypeFamilies"
    "-XDataKinds"
    "-XFunctionalDependencies"
    "-XUndecidableInstances"
  ];

  # Passing --hint at all is what turns off hlint's search for a `.hlint.yaml`
  # beside the sources, so the shared ruleset and the local file are the only
  # ones that apply. The local one comes second because later files win.
  arguments =
    lib.concatStringsSep " " (
      [
        "--hint=${hlint-config}"
        "--hint=${local}"
      ]
      ++ language
      ++ directories
    );

  # hlint quotes the code it complains about, and the code is not all ASCII;
  # without a UTF-8 locale it dies on the first such character it prints.
  command = "LANG=C.UTF-8 ${hlint}/bin/hlint ${arguments}";
in
{
  # The `lint` task in `nix/tasks.nix` is this check run on its own.
  flake.output.checks = lib.optionalAttrs lintable {
    lint-check = pkgs.runCommandLocal "lint-check" { } ''
      cd ${source}
      ${command}
      touch $out
    '';
  };
}
