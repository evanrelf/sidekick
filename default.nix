let
  pkgs = import ./nix/pkgs.nix;

in
{
  sidekick = pkgs.haskellPackages.sidekick;
  sidekick-ghci = pkgs.haskellPackages.sidekick-ghci;
  sidekick-ghci-json = pkgs.haskellPackages.sidekick-ghci-json;
  sidekick-ghci-parsers = pkgs.haskellPackages.sidekick-ghci-parsers;
}
