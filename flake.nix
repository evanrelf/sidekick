{
  description = "sidekick";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    haskell-overlay.url = "github:evanrelf/haskell-overlay";
    nixpkgs.url = "github:NixOS/nixpkgs";
  };

  outputs = inputs@{ flake-utils, nixpkgs, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs =
          import nixpkgs {
            inherit system;
            overlays = [
              inputs.haskell-overlay.overlay
              (import ./nix/haskell-packages.nix)
            ];
          };
      in
      rec {
        packages = {
          default = pkgs.haskellPackages.sidekick-ghci;

          inherit (pkgs.haskellPackages)
            sidekick-ghci
            sidekick-shell
            ;
        };

        devShells.default = packages.sidekick-shell;
      }
    );
}
