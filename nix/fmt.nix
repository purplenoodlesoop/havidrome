{
  ai-haskell-linter,
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (ai-haskell-linter.packages.${pkgs.stdenv.hostPlatform.system}) fourmolu fourmolu-config;
  inherit (pkgs) git nixfmt;
  inherit (lib.fileset) toSource unions;

  # Importing nixpkgs for a system havidrome is not built for throws, and the
  # linter flake makes no exception for it: neither formatter can be reached
  # there, and there is no Haskell to format either way. Both the check and
  # the task are absent there, as the dev shell already is.
  formattable = config.flake.packages ? havidrome;

  root = ../.;

  # The two packages, handed over whole. fourmolu walks a directory and takes
  # the `.hs` files under it, so a new module -- or a new directory of them --
  # is formatted without being named here, and nothing outside these two
  # directories is Haskell. The `.cabal` files come along because fourmolu
  # reads the language and the extensions a file is compiled with out of
  # them, and mis-parses the code without them.
  directories = [
    "core"
    "tui"
  ];

  # fourmolu and its configuration both come from the shared linter flake,
  # the same place the hlint ruleset comes from, so what "formatted" means is
  # the agreed thing and not whatever the machine has.
  #
  # fourmolu quotes the code it reformats, and the code is not all ASCII;
  # without a UTF-8 locale it dies on the first such character it prints.
  formatHaskell =
    mode:
    "LANG=C.UTF-8 ${fourmolu}/bin/fourmolu --config ${fourmolu-config} --mode ${mode} ${lib.concatStringsSep " " directories}";

  # `nixfmt` is the formatter nixpkgs itself is written with. The glob is the
  # shell's, expanded where the command runs -- the check's source tree or
  # the repository -- so a new module under `nix/` needs no change here.
  formatNix = arguments: "${nixfmt}/bin/nixfmt ${arguments} flake.nix nix/*.nix";

  source = toSource {
    inherit root;
    fileset = unions (
      [
        (root + "/flake.nix")
        (root + "/nix")
      ]
      ++ map (directory: root + "/${directory}") directories
    );
  };
in
{
  # The task and the check are written side by side on purpose: they are the
  # same two commands in the two modes, so neither can come to read a
  # different configuration or cover different files than the other.
  flake.output.checks = lib.optionalAttrs formattable {
    fmt-check = pkgs.runCommandLocal "fmt-check" { } ''
      cd ${source}
      ${formatHaskell "check"}
      ${formatNix "--check"}
      touch $out
    '';
  };

  tasks = lib.optionalAttrs formattable {
    fmt = {
      description = "Format the Haskell and the Nix in place";
      body = ''
        set -euo pipefail
        cd "$(${git}/bin/git rev-parse --show-toplevel)"
        ${formatHaskell "inplace"}
        ${formatNix ""}
      '';
    };
  };
}
