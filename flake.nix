{
  description = "githud flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?rev=0bf298df24f721a7f85c580339fb7eeff64b927c";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    let
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = hfinal: hprev:
            prev.haskell.packageOverrides hfinal hprev // {
              githud =
                let
                  src = builtins.path {
                    name = "githud-src";
                    path = ./.;
                  };
                in
                hfinal.callCabal2nix "githud" src { };
            };
        };
        githud = final.haskell.lib.justStaticExecutables final.haskellPackages.githud;
      };
    in
    {
      overlays.default = overlay;
    } // flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ self.overlays.default ];
        };
      in
      {
        packages.default = pkgs.githud;
      });
}
