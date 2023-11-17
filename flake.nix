{
  description = "githud flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
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
        devShells.default = pkgs.mkShell {
          buildInputs = [
            (pkgs.haskell.lib.justStaticExecutables pkgs.haskellPackages.ghcid)
            (pkgs.cabal-install)
          ];
          inputsFrom = [ self.packages.${system}.default.env ];
        };
      });
}
