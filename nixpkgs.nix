# To upgrade
# - Find latest commit on howoldis.herokuapp.com
# - execute nix-prefetch-url --unpack https://github.com/nixos/nixpkgs/archive/SHORT_COMMIT.tar.gz
# - replace below rev with the SHORT_COMMIT and sha256 by the hash output by the previous command
let
  rev = "0bf298df24f7";
  sha256 = "0kdx3pz0l422d0vvvj3h8mnq65jcg2scb13dc1z1lg2a8cln842z";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
