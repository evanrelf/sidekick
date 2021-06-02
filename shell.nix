let
  pkgs = import ./nix/nixpkgs.nix {};

  sidekick = import ./default.nix;

in
  sidekick.env.overrideAttrs (old: {
    buildInputs = with pkgs; old.buildInputs ++ [
      cabal-install
      ghcid
    ];
  })
