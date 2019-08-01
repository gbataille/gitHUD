let
  rev = "88d9f776091";
  sha256 = "0z8a0g69fmbbzi77jhvhwafv73dn5fg3gsr0q828lss6j5qpx995";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
