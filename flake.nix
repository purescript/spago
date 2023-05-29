{
  description = "PureScript package manager";

  inputs = {
    utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    spago-nix.url = "github:thomashoneyman/spago-nix";
  };

  outputs = inputs: let
    utils.supportedSystems = ["x86_64-linux" "x86_64-darwin" "aarch64-darwin"];
    utils.eachSupportedSystem = inputs.utils.lib.eachSystem utils.supportedSystems;

    mkFlakePackages = pkgs: let
      npmDependencies = pkgs.spago-npm-dependencies {src = ./spaghetto;};
      workspaces = pkgs.spago-lock {src = ./spaghetto;};
    in {
      default = pkgs.stdenv.mkDerivation rec {
        name = "spago";
        version = "0.93.4";
        src = ./spaghetto/bin;
        phases = ["buildPhase" "installPhase"];
        nativeBuildInputs = [pkgs.purescript pkgs.esbuild pkgs.nodejs];
        # A dummy build info so we can bootstrap Spago. The only thing that
        # MUST be correct is the version.
        #
        # TODO: This ain't great.
        buildInfo = pkgs.writeText "BuildInfo.purs" ''
          module Spago.Generated.BuildInfo where
          buildInfo :: { packages :: Array { name :: String, version :: String }, pursVersion :: String, spagoVersion :: String }
          buildInfo =
            { packages: []
            , pursVersion: "0.15.8"
            , spagoVersion: "${version}"
            }
        '';
        buildPhase = ''
          # Make sure node dependencies are available before bundling
          ln -s ${npmDependencies}/js/node_modules .

          # We need a dummy build-info file to bootstrap Spago
          # (this file is normally created by Spago itself)
          mkdir .spago && cp ${buildInfo} .spago/BuildInfo.purs

          # No glob expansion for the compiler...
          set -f
          purs compile $src/src/**/*.purs ${workspaces.spago-bin.dependencies.globs} .spago/BuildInfo.purs
          set +f

          # Technically we should bundle Spago using Spago, via e.g.
          # the index.dev.js file. But we can't do that until Spago has an
          # offline mode (it otherwise tries to clone various repos like the
          # registry index). Instead we've manually reproduced the build config
          # arguments to esbuild here.
          esbuild \
            --bundle ./output/Main/index.js \
            --outfile=bundle.js \
            --platform=node
        '';
        installPhase = ''
          mkdir $out
          cp bundle.js $out
        '';
      };
    };

    mkFlakeApps = pkgs: packages: {
      default = {
        type = "app";
        program = "${
          pkgs.writeShellScriptBin "spago-run" ''
            echo "$@"
            ${pkgs.nodejs}/bin/node --eval 'require("${packages.default}/bundle.js").main()' "$@"
          ''
        }/bin/spago-run";
      };
    };

    mkFlakeDevShells = pkgs: pursPkgs: {
      default = pkgs.mkShell {
        buildInputs = [pursPkgs.purs pursPkgs.spago-next pkgs.esbuild];
      };
    };

    mkFlakeOutput = system: let
      pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [inputs.spago-nix.overlay];
      };
      pursPkgs = pkgs.callPackage inputs.easy-purescript-nix {};
    in rec {
      packages = mkFlakePackages pkgs;
      apps = mkFlakeApps pkgs packages;
      devShells = mkFlakeDevShells pkgs pursPkgs;
    };

    systemOutputs = utils.eachSupportedSystem mkFlakeOutput;
  in
    systemOutputs;
}
