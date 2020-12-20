{ packages ? {}  # Add or replace Haskell packages
, overrides ? {} # Override existing Haskell packages
, hackage ? null # Specify revision of all-cabal-hashes
}:

pkgsFinal: pkgsPrev:

let
  packagesExtension = pkgsPrev.haskell.lib.packageSourceOverrides packages;


  overridesExtension = haskellPackagesNew: haskellPackagesOld:
    let
      applyOverride = name: fn:
        pkgsPrev.haskell.lib.overrideCabal haskellPackagesOld."${name}" fn;
    in
      pkgsPrev.lib.mapAttrs applyOverride overrides;


  haskellPackages =
    pkgsPrev.haskellPackages.override (old: {
      overrides =
        pkgsPrev.lib.fold
          pkgsPrev.lib.composeExtensions
          (old.overrides or (_: _: {}))
          [ packagesExtension
            overridesExtension
          ];
    });


  all-cabal-hashes =
    if builtins.isNull hackage then
      pkgsPrev.all-cabal-hashes
    else
      pkgsPrev.fetchurl {
        url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/${hackage.rev}.tar.gz";
        sha256 = hackage.sha256;
      };

in
  { inherit
      haskellPackages
      all-cabal-hashes
    ;
  }
