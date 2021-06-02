let
  pkgs = import ./nix/pkgs.nix;

in
{
  sidekick = pkgs.haskellPackages.sidekick;
  sidekick-ghci = pkgs.haskellPackages.sidekick-ghci;
}
