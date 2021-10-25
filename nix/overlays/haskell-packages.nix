{ compiler }:

pkgsFinal: pkgsPrev:

let
  haskellOverlay = import ../lib/haskell-overlay.nix pkgsFinal pkgsPrev;

  source = path:
    let
      name = builtins.baseNameOf path;
      root = pkgsPrev.lib.gitignoreSource (builtins.dirOf path);
    in
    pkgsPrev.runCommandLocal "${name}-source" { } ''
      cd ${root}
      ${pkgsFinal.coreutils}/bin/cp -r --dereference ${name} $out
    '';

in
haskellOverlay.mkOverlay {
  inherit compiler;

  hackage = {
    rev = "74d57b6403ac6b672eacaac9fd68fd7edf54d937";
    sha256 = "18xryn7b6wqlizrfvhnv3jvpgmmlzrx9dkv86r4g8x886ggv23fh";
  };

  extensions = [
    (haskellOverlay.sources (haskellPackagesFinal: haskellPackagesPrev: {
      "sidekick" = source ../../sidekick;
      "sidekick-ghci" = source ../../sidekick-ghci;
      "sidekick-ghci-json" = source ../../sidekick-ghci-json;
      "sidekick-ghci-parsers" = source ../../sidekick-ghci-parsers;
    }))

    (haskellPackagesFinal: haskellPackagesPrev: {
      "optics" = haskellPackagesFinal."optics_0_4";
      "optics-core" = haskellPackagesFinal."optics-core_0_4";
      "optics-extra" = haskellPackagesFinal."optics-extra_0_4";
      "optics-th" = haskellPackagesFinal."optics-th_0_4";
      "streamly" = haskellPackagesFinal."streamly_0_8_0";
    })

    (haskellOverlay.overrideCabal (haskellPackagesFinal: haskellPackagesPrev:
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

        "streamly" = prev: {
          # `cabal2nix` doesn't add `Cocoa` to `streamly-0.8.0`'s `buildInputs`
          # automatically.
          # https://github.com/NixOS/cabal2nix/issues/470
          libraryHaskellDepends = (prev.libraryHaskellDepends or [ ]) ++
            pkgsPrev.lib.optionals
              pkgsPrev.stdenv.isDarwin
              [ pkgsFinal.darwin.apple_sdk.frameworks.Cocoa ];
        };
      }))
  ];
}
