let
  pkgs = import ./nix/pkgs.nix;

in
pkgs.haskellPackages.shellFor {
  packages = p: [
    p.sidekick
    p.sidekick-ghci
    p.sidekick-ghci-json
    p.sidekick-ghci-parsers
  ];

  buildInputs = [
    pkgs.cabal-install
    pkgs.ghcid
  ];
}
