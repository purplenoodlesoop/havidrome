{
  description = "havidrome — a CLI player for a Navidrome server.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    core-flake = {
      url = "github:purplenoodlesoop/core-flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # The shared hlint ruleset, and the hlint that reads it. Both come from
    # here so that neither the rules nor the linter's version is whatever the
    # machine happens to have.
    ai-haskell-linter = {
      url = "github:purplenoodlesoop/ai_haskell_linter";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { ai-haskell-linter, core-flake, ... }:
    core-flake.lib.evalFlake {
      # nixpkgs-unstable has dropped x86_64-darwin, and importing it for that
      # system throws on sight. Forcing the import past that throw is what
      # keeps the flake evaluable there; it offers nothing of havidrome.
      config.allowDeprecatedx86_64Darwin = "force";

      specialArgs = { inherit ai-haskell-linter; };

      perSystem.imports = [
        # Every operation this repository has is a declared task, and this
        # module is what turns the declarations in `nix/tasks.nix` into apps
        # and into commands on the dev shell's PATH.
        core-flake.nixosModules.tasks
        ./nix/havidrome.nix
        ./nix/shell.nix
        ./nix/checks.nix
        ./nix/tasks.nix
        ./nix/lint.nix
      ];
    };
}
