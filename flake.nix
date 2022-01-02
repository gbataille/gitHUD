{
  description = "githud flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?rev=0bf298df24f721a7f85c580339fb7eeff64b927c";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
      in
      {
        defaultPackage =
          pkgs.haskellPackages.developPackage {
            root = ./.;
            name = "githud";
          };
      });
}
