{ nixpkgs ? ./nixpkgs.nix }:
with import nixpkgs {};

haskellPackages.developPackage {
  root = ./.;
  name = "githud";

  # TODO: remove when nixpkgs is updated
  overrides = self : super : {
    daemons = super.callHackageDirect {
      pkg = "daemons";
      ver = "0.3.0";
      sha256 = "05wsgf4qn3lpf81x70ich3wbvfhmp04j45pnda2sw0dr2h4dli73";
    } {};
  };
}
