args:

let
  # master on 2021-07-14
  rev = "aee00346d85676cebd22484547d3795c0ca0ac0f";
  sha256 = "14ywi2m8dwlf82rlyb6a261i2njm520vah1ic0fz0aw0nba8rqzg";

  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };

in
import nixpkgs ({ config = { }; } // args)
