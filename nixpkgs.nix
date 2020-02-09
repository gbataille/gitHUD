# To upgrade
# - Find latest commit on howoldis.herokuapp.com
# - execute nix-prefetch-url --unpack https://github.com/nixos/nixpkgs/archive/SHORT_COMMIT.tar.gz
# - replace below rev with the SHORT_COMMIT and sha256 by the hash output by the previous command
let
  rev = "0c960262d15";
  sha256 = "0d7ms4dxbxvd6f8zrgymr6njvka54fppph1mrjjlcan7y0dhi5rb";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
