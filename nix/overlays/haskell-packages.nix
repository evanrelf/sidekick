{ compiler }:

pkgsFinal: pkgsPrev:

let
  overrideHaskellPackages = attrs:
    import ../lib/override-haskell-packages.nix attrs pkgsFinal pkgsPrev;

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
overrideHaskellPackages {
  inherit compiler;

  packages = haskellPackagesFinal: haskellPackagesPrev: {
    "sidekick" = source ../../sidekick;
    "sidekick-ghci" = source ../../sidekick-ghci;
    "sidekick-ghci-json" = source ../../sidekick-ghci-json;
    "sidekick-ghci-parsers" = source ../../sidekick-ghci-parsers;

    "optics" = "0.4";
    "optics-core" = "0.4";
    "optics-extra" = "0.4";
    "optics-th" = "0.4";
    "streamly" = "0.8.0";
  };

  overrideCabal = haskellPackagesFinal: haskellPackagesPrev:
    let
      enableFusionPlugin = old: {
        configureFlags = (old.configureFlags or [ ]) ++ [ "-ffusion-plugin" ];
        libraryHaskellDepends = (old.libraryHaskellDepends or [ ]) ++ [
          haskellPackagesFinal.fusion-plugin
        ];
      };
    in
    {
      "sidekick" = old: enableFusionPlugin old;

      # TODO: Fix tests failing in Nix
      "sidekick-ghci" = old: enableFusionPlugin old // {
        doCheck = false;
      };

      "streamly" = old: {
        # `cabal2nix` doesn't add `Cocoa` to `streamly-0.8.0`'s `buildInputs`
        # automatically.
        # https://github.com/NixOS/cabal2nix/issues/470
        libraryHaskellDepends = (old.libraryHaskellDepends or [ ]) ++
          pkgsPrev.lib.optionals
            pkgsPrev.stdenv.isDarwin
            [ pkgsFinal.darwin.apple_sdk.frameworks.Cocoa ];
      };
    };

  hackage = {
    rev = "124d577814bdcb2c29d5df1d99c25109c0ec5866";
    sha256 = "0zia0ksk8wm9fzp84f8ws7zy4bsa2f84kbjir9lx1b6hglvxga48";
  };
}
