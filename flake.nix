{
  description = "AOC 2023";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, utils, ... }:
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default =
          pkgs.haskellPackages.shellFor {
            name = "aoc2023";
            packages = _: [ ];
            nativeBuildInputs = with pkgs; [ cabal-install ghcid ];
          };
      });
}
