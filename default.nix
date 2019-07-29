with import (fetchTarball {
   url = https://github.com/nixos/nixpkgs/archive/beff2f8d75e.tar.gz;
   sha256 = "1av1m2mibv9dgfrjv9r8n3ih9dyb0wi594s5xb4c135v121jpzs3";
}) {};

haskellPackages.developPackage {
  root = ./.;
  name = "githud";
}
