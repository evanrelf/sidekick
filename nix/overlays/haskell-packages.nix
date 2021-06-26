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

  packages = {
    "sidekick" = source ../../sidekick;
    "sidekick-ghci" = source ../../sidekick-ghci;
    "sidekick-ghci-json" = source ../../sidekick-ghci-json;
    "sidekick-ghci-parsers" = source ../../sidekick-ghci-parsers;

    "optics" = "0.4";
    "optics-core" = "0.4";
    "optics-extra" = "0.4";
    "optics-th" = "0.4";
  };

  overrideCabal = {
    # TODO: Fix tests failing in Nix
    "sidekick-ghci" = old: { doCheck = false; };

    "streamly-fsnotify" = old: {
      broken = false;
      jailbreak = true;
    };
  };

  hackage = {
    rev = "09f25d55b8df341d0e42c152d9703af72a460873";
    sha256 = "1aaph1302x6r16xdwq4c7n6ry976br4klr0i36ifm7k5znm47z1j";
  };
}
