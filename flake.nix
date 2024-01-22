{
  description = "File Manager Cli";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "utils";
    };
  };

  outputs = {
    self,
    nixpkgs,
    utils,
    pre-commit-hooks,
    ...
  }:
    utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          (final: _: {project.haskellPackages = final.haskell.packages.ghc948;})
        ];
      };
      haskellPackages = pkgs.project.haskellPackages.extend (final: prev: {
        aoc2023 = prev.callCabal2nix "aoc2023" ./. {};
      });
    in {
      checks = {
        pre-commit-check = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            alejandra.enable = true;
            hlint.enable = true;
            cabal-fmt.enable = true;
            fourmolu.enable = true;
          };
        };
      };
      devShells.default =
        haskellPackages.shellFor
        {
          name = "aoc2023";
          packages = p: [p.aoc2023];
          withHoogle = true;
          inherit (self.checks.${system}.pre-commit-check) shellHook;
          buildInputs = with pkgs; [zlib.dev];
          nativeBuildInputs = builtins.attrValues {
            # Code styles:
            inherit (pre-commit-hooks.packages.${system}) fourmolu cabal-fmt hlint alejandra;
            inherit (pkgs.python3Packages) yamllint;
            inherit (pkgs.nodePackages) prettier;
            # Command line tools:
            inherit (pkgs) entr ghcid git;
            # Language servers:
            inherit (haskellPackages) haskell-language-server;
            # Package managers:
            inherit (pkgs) cabal-install;
          };
        };
    });
}
