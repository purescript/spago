{ lib
, stdenv
, slimlock
, python3
, purix
, purs
, esbuild
, writeText
, writeShellScriptBin
, nodejs
}:
let
  fs = lib.fileset;
  packageLock = (slimlock.buildPackageLock {
    src = fs.toSource {
      root = ./.;
      fileset = fs.unions [
        ./package-lock.json
        ./package.json
      ];
    };
  }).overrideAttrs (x: {
    nativeBuildInputs = (x.nativeBuildInputs or [ ]) ++ [ python3 ];
  });

  spagoLock = purix.buildSpagoLock {
    src = fs.toSource {
      root = ./.;
      fileset = fs.unions [
        ./bin
        ./core
        ./docs-search
        ./src
        ./spago.lock
        ./spago.yaml
      ];
    };
  };

  spago-app = stdenv.mkDerivation {
    name = "spago-app";

    src = fs.toSource {
      root = ./.;
      fileset = fs.unions [
        ./bin/index.dev.js
        ./package.json
        ./package-lock.json
      ];
    };

    nativeBuildInputs = [ esbuild ];

    buildPhase = ''
      echo "Linking ..."
      ln -s ${packageLock}/js/node_modules .
      ln -s ${spagoLock.spago-bin}/output .
      esbuild bin/index.dev.js \
        --bundle \
        --outfile=app.js \
        --platform=node \
        --format=esm \
        --packages=external
    '';

    installPhase = ''
      mkdir $out
      cp app.js $out
      cp package.json package-lock.json $out
      ln -s ${packageLock}/js/node_modules $out
      ln -s $out/app.js $out/spago
    '';
  };

  spago-script = writeShellScriptBin "spago" ''
    ${nodejs}/bin/node ${spago-app}/spago "$@"
  '';

  spago = stdenv.mkDerivation {
    name = "spago";
    phases = [ "installPhase" ];
    installPhase = ''
      mkdir -p $out
      
      SPAGO="$out/bin/spago"
      ln -s ${spago-script}/bin $out

      BASH_COMPLETIONS=$out/share/bash-completion/completions/
      mkdir -p $BASH_COMPLETIONS
      $SPAGO --bash-completion-script $SPAGO > $BASH_COMPLETIONS/spago
    '';
  };
in
spago