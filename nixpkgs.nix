let
  rev = "40be1827ee6";
  sha256 = "094vm2sl4cs5pl8s644lfz0rf5khw20mjwqd3s0myr198z6mwp0j";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
