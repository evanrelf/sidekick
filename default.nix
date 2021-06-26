{ compiler ? "ghc8104" }:

let
  pkgs = import ./nix/pkgs.nix { inherit compiler; };

in
{
  sidekick = pkgs.haskellPackages.sidekick;
  sidekick-ghci = pkgs.haskellPackages.sidekick-ghci;
  sidekick-ghci-json = pkgs.haskellPackages.sidekick-ghci-json;
  sidekick-ghci-parsers = pkgs.haskellPackages.sidekick-ghci-parsers;
}
