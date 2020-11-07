{ nixpkgs ? ./nixpkgs.nix }:
with import nixpkgs {};

haskellPackages.developPackage {
  root = ./.;
  name = "githud";
}
