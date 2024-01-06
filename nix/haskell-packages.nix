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
      "sidekick-hie" = ../sidekick-hie;
    }))

    (haskell-overlay.overrideCabal (haskellPackagesFinal: haskellPackagesPrev: {
      "sidekick-ghci" = {
        # TODO: Fix tests failing in Nix
        checkPhase = "";
      };
      "sidekick-hie" = {
        # TODO: Get tests working in Nix
        checkPhase = "";
      };
    }))

    (haskellPackagesFinal: haskellPackagesPrev: {
      "sidekick-shell" =
        haskellPackagesFinal.shellFor {
          packages = p: [
            p.sidekick
            p.sidekick-ghci
            p.sidekick-hie
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
