{
  ai-haskell-linter,
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (ai-haskell-linter.packages.${pkgs.system}) hlint hlint-config;
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

  source = toSource {
    inherit root;
    fileset = unions (map (directory: root + "/${directory}") directories);
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
  # beside the sources, so the shared ruleset is the only one that applies.
  # A second --hint would be how a single module is let off a rule: later
  # files win. There is none, so nothing is.
  arguments = lib.concatStringsSep " " ([ "--hint=${hlint-config}" ] ++ language ++ directories);

  # hlint quotes the code it complains about, and the code is not all ASCII;
  # without a UTF-8 locale it dies on the first such character it prints.
  command = "LANG=C.UTF-8 ${hlint}/bin/hlint ${arguments}";
in
{
  # The check and the task run the one command, so `nix flake check` and
  # `lint` cannot disagree about what the ruleset says.
  flake.output.checks = lib.optionalAttrs lintable {
    lint-check = pkgs.runCommandLocal "lint-check" { } ''
      cd ${source}
      ${command}
      touch $out
    '';
  };

  tasks = lib.optionalAttrs lintable {
    lint = {
      description = "Lint both packages against the shared hlint ruleset";
      body = ''
        set -euo pipefail
        cd "$(git rev-parse --show-toplevel)"
        ${command}
      '';
    };
  };
}
