let
  haskellPackagesOverlay =
    import ./nix/lib/override-haskell-packages.nix {
      packages = {
        "sidekick" = pkgs.nix-gitignore.gitignoreSource [ ./.nixignore ] ./.;
        "optics" = "0.4";
        "optics-core" = "0.4";
        "optics-extra" = "0.4";
        "optics-th" = "0.4";
      };
      overrideCabal = {
        "streamly-fsnotify" = old: {
          broken = false;
          jailbreak = true;
        };
      };
      hackage = {
        rev = "feae602db08e48b1d1a716fc9c4c8348aa5df17a";
        sha256 = "0m7qr2nmfj6lbf9h6wfyapphcqwarqw3319zkdxqwzhbis81hlcb";
      };
    };


  pkgs = import ./nix/nixpkgs.nix { overlays = [ haskellPackagesOverlay ]; };

in
  pkgs.haskellPackages.sidekick
