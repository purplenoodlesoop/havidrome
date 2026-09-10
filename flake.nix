{
  description = "havidrome — a CLI player for a Navidrome server.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    core-flake = {
      url = "github:purplenoodlesoop/core-flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { core-flake, ... }:
    core-flake.lib.evalFlake {
      perSystem = ./nix/havidrome.nix;
    };
}
