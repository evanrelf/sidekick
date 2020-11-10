let
  haskellPackagesOverlay =
    import ./nix/override-haskell-packages.nix {
      packages = {
        "sidecar" = pkgs.nix-gitignore.gitignoreSource [ ./.nixignore ] ./.;
      };
      overrides = {
        "relude" = oldCabal: {
          patches = (oldCabal.patches or []) ++ [ ./nix/patches/relude.patch ];
        };
      };
    };


  pkgs = import ./nix/nixpkgs.nix { overlays = [ haskellPackagesOverlay ]; };


  sidecar = pkgs.haskellPackages.sidecar;


  executable = pkgs.haskell.lib.justStaticExecutables sidecar;


  shell =
    sidecar.env.overrideAttrs (old: {
      buildInputs = with pkgs; old.buildInputs ++ [
        cabal-install
        ghcid
        hlint
      ];
    });

in
  { inherit
      sidecar
      executable
      shell
    ;
  }
