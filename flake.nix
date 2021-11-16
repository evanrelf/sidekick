{
  description = "sidekick";

  inputs = {
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
    haskell-overlay.url = "github:evanrelf/haskell-overlay";
    gitignore = {
      url = "github:hercules-ci/gitignore.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , gitignore
    , haskell-overlay
    , ...
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs =
        import nixpkgs {
          inherit system;
          overlays = [
            haskell-overlay.overlay
            gitignore.overlay
            (import ./nix/overlays/haskell-packages.nix)
          ];
        };
    in
    rec {
      packages = {
        inherit (pkgs.haskellPackages)
          sidekick
          sidekick-ghci
          sidekick-ghci-json
          sidekick-ghci-parsers
          sidekick-shell
          ;
      };

      defaultPackage = packages.sidekick;

      devShell = packages.sidekick-shell;
    }
    );
}
