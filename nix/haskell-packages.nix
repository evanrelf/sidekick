pkgsFinal: pkgsPrev:

let
  inherit (pkgsPrev) haskell-overlay;

in
haskell-overlay.mkOverlay
{
  extensions = [
    (haskell-overlay.sources (haskellPackagesFinal: haskellPackagesPrev: {
      "sidekick" = ../sidekick;
      "sidekick-ghci" = ../sidekick-ghci;
      "sidekick-ghci-json" = ../sidekick-ghci-json;
      "sidekick-ghci-parsers" = ../sidekick-ghci-parsers;
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
