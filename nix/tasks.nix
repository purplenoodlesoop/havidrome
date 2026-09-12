{ pkgs, ... }:
let
  inherit (pkgs) git nix;
  inherit (pkgs.stdenv.hostPlatform) system;

  # Every task begins the same way: the shell aborts on the first failure, the
  # working directory is the repository root whatever directory the task was
  # run from, and every binary is named by its store path. What a task does
  # must not depend on what happens to be installed on the machine running it,
  # nor on where the caller stood.
  task = description: lines: {
    inherit description;
    body = ''
      set -euo pipefail
      cd "$(${git}/bin/git rev-parse --show-toplevel)"
      ${lines}
    '';
  };

  # `--no-link` because a task is run to learn that something builds, not to
  # leave a `result` symlink in the tree.
  nixBuild = target: "${nix}/bin/nix build --no-link --print-build-logs ${target}";
in
{
  # The player itself is the app `nix run .#havidrome`, so no task runs it.
  tasks = {
    build = task "Build the player" (nixBuild ".#havidrome");

    build-debug = task "Build the debug build of the player" (nixBuild ".#havidrome-debug");

    # The core is the half with no audio and no terminal in it, and the check
    # that builds it is what compiles its test suite: a machine that cannot
    # build the player -- a macOS one, as long as the shell's tests want a
    # real player -- can still run this.
    core = task "Build and test the core on its own, which works on macOS" (
      nixBuild ".#checks.${system}.havidrome-core-test"
    );

    check = task "Run the whole gate" "${nix}/bin/nix flake check --print-build-logs";
  };
}
