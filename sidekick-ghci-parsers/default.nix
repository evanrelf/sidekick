let
  pkgs = import ../nix/pkgs.nix;

in
pkgs.haskellPackages.sidekick-ghci-parsers
