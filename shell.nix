let
  pkgs = import ./nix/pkgs.nix;

in
pkgs.haskellPackages.shellFor {
  packages = p: [
    p.sidekick
    p.sidekick-ghci
  ];

  buildInputs = [
    pkgs.cabal-install
    pkgs.ghcid
  ];
}
