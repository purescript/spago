
# This builds a statically-linked `spago` executable.
# It currently only works on Linux.

let
  nixpkgs-src = builtins.fetchTarball {
    # This is one of the last commits from https://github.com/NixOS/nixpkgs/pull/169513.
    # This commit is used because it is for the `haskell-updates` branch.
    # Hydra builds a GHC for static linking on this branch, so we can get
    # most dependencies from the NixOS cache.
    url = "https://github.com/NixOS/nixpkgs/archive/999a0e87f9b1792e229c53602e80bda6dd52c105.tar.gz";
    sha256 = "1p0vgmdz884gld4p2zcdy7g0w7xznc4kxj80wf5pk2qdxicbnscy";
  };

  spago-overlay = final: prev: {

    myhaskell = prev.haskell // {
      packages = prev.haskell.packages // {
        native-bignum = prev.haskell.packages.native-bignum // {
          ghc902 = prev.haskell.packages.native-bignum.ghc902.override (oldAttrs: {
            overrides =
              final.lib.composeExtensions
                (oldAttrs.overrides or (_: _: {}))
                (hfinal: hprev: {

                  aeson = hfinal.aeson_1_5_6_0;

                  # bower-json needs aeson_1_5_6_0 and is marked broken without it.
                  bower-json = final.haskell.lib.doDistribute (final.haskell.lib.markUnbroken hprev.bower-json);

                  spago =
                    let
                      filesToIgnore = [
                        "default.nix"
                        ".git"
                        ".github"
                        "Makefile"
                        "result"
                        ".stack-work"
                        "stack.yaml"
                      ];

                      src = builtins.path {
                        name = "spago-src";
                        path = ./.;
                        filter = path: type:
                          with final.lib;
                          ! elem (baseNameOf path) filesToIgnore &&
                          ! any (flip hasPrefix (baseNameOf path)) [ "dist" ".ghc" ];
                      };


                      docsSearchApp_0_0_10 = final.fetchurl {
                        url = "https://github.com/purescript/purescript-docs-search/releases/download/v0.0.10/docs-search-app.js";
                        sha256 = "0m5ah29x290r0zk19hx2wix2djy7bs4plh9kvjz6bs9r45x25pa5";
                      };

                      docsSearchApp_0_0_11 = final.fetchurl {
                        url = "https://github.com/purescript/purescript-docs-search/releases/download/v0.0.11/docs-search-app.js";
                        sha256 = "17qngsdxfg96cka1cgrl3zdrpal8ll6vyhhnazqm4hwj16ywjm02";
                      };

                      purescriptDocsSearch_0_0_10 = final.fetchurl {
                        url = "https://github.com/purescript/purescript-docs-search/releases/download/v0.0.10/purescript-docs-search";
                        sha256 = "0wc1zyhli4m2yykc6i0crm048gyizxh7b81n8xc4yb7ibjqwhyj3";
                      };

                      purescriptDocsSearch_0_0_11 = final.fetchurl {
                        url = "https://github.com/purescript/purescript-docs-search/releases/download/v0.0.11/purescript-docs-search";
                        sha256 = "1hjdprm990vyxz86fgq14ajn0lkams7i00h8k2i2g1a0hjdwppq6";
                      };

                      spagoDrvFromCabal2nix =
                        final.haskell.lib.overrideCabal
                          (hfinal.callCabal2nix "spago" src {})
                          (drv: {
                            doCheck = false;
                            postUnpack = (drv.postUnpack or "") + ''
                              # Spago includes the following two files directly into the binary
                              # with Template Haskell.  They are fetched at build-time from the
                              # `purescript-docs-search` repo above.  If they cannot be fetched at
                              # build-time, they are pulled in from the `templates/` directory in
                              # the spago source.
                              #
                              # However, they are not actually available in the spago source, so they
                              # need to fetched with nix and put in the correct place.
                              # https://github.com/spacchetti/spago/issues/510
                              cp ${docsSearchApp_0_0_10} "$sourceRoot/templates/docs-search-app-0.0.10.js"
                              cp ${docsSearchApp_0_0_11} "$sourceRoot/templates/docs-search-app-0.0.11.js"
                              cp ${purescriptDocsSearch_0_0_10} "$sourceRoot/templates/purescript-docs-search-0.0.10"
                              cp ${purescriptDocsSearch_0_0_11} "$sourceRoot/templates/purescript-docs-search-0.0.11"

                              # For some weird reason, on Darwin, the open(2) call to embed these files
                              # requires write permissions. The easiest resolution is just to permit that
                              # (doesn't cause any harm on other systems).
                              chmod u+w \
                                "$sourceRoot/templates/docs-search-app-0.0.10.js" \
                                "$sourceRoot/templates/purescript-docs-search-0.0.10" \
                                "$sourceRoot/templates/docs-search-app-0.0.11.js" \
                                "$sourceRoot/templates/purescript-docs-search-0.0.11"
                            '';
                          });
                    in
                    final.haskell.lib.compose.generateOptparseApplicativeCompletion
                      "spago"
                      spagoDrvFromCabal2nix;
                });
          });
        };
      };
    };

    spago-haskell-static-pkg-set = final.pkgsStatic.myhaskell.packages.native-bignum.ghc902;

    spago = final.haskell.lib.justStaticExecutables final.spago-haskell-static-pkg-set.spago;
  };

  nixpkgs = import nixpkgs-src { overlays = [ spago-overlay ]; };

in

nixpkgs.spago
