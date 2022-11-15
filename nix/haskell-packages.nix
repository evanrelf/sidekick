pkgsFinal: pkgsPrev:

let
  inherit (pkgsPrev) haskell-overlay;

  source = path:
    let
      name = builtins.baseNameOf path;
      root = pkgsPrev.gitignoreSource (builtins.dirOf path);
    in
    pkgsPrev.runCommandLocal "${name}-source" { } ''
      cd ${root}
      ${pkgsFinal.coreutils}/bin/cp -r --dereference ${name} $out
    '';

in
haskell-overlay.mkOverlay
{
  extensions = [
    (haskell-overlay.sources (haskellPackagesFinal: haskellPackagesPrev: {
      "lsp" = "1.5.0.0";
      "lsp-types" = "1.5.0.0";
      "sidekick" = source ../sidekick;
      "sidekick-ghci" = source ../sidekick-ghci;
      "sidekick-ghci-json" = source ../sidekick-ghci-json;
      "sidekick-ghci-parsers" = source ../sidekick-ghci-parsers;
    }))

    (haskell-overlay.overrideCabal (haskellPackagesFinal: haskellPackagesPrev:
      let
        enableFusionPlugin = prev: {
          configureFlags = (prev.configureFlags or [ ]) ++ [ "-ffusion-plugin" ];
          libraryHaskellDepends = (prev.libraryHaskellDepends or [ ]) ++ [
            haskellPackagesFinal.fusion-plugin
          ];
        };
      in
      {
        "sidekick" = prev: enableFusionPlugin prev;

        # TODO: Fix tests failing in Nix
        "sidekick-ghci" = prev: enableFusionPlugin prev // {
          doCheck = false;
        };
      })
    )

    (haskellPackagesFinal: haskellPackagesPrev: {
      "sidekick-shell" =
        haskellPackagesFinal.shellFor {
          packages = p: [
            p.sidekick
            p.sidekick-ghci
            p.sidekick-ghci-json
            p.sidekick-ghci-parsers
          ];

          buildInputs = [
            pkgsFinal.cabal-install
            pkgsFinal.ghcid
          ];
        };
    })
  ];
}
  pkgsFinal
  pkgsPrev
