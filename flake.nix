{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    purescript-overlay.url = "github:deemp/purescript-overlay/main";
    purescript-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      supportedSystems = [ "aarch64-linux" "x86_64-linux" "aarch64-darwin" "x86_64-darwin" ];

      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = builtins.attrValues self.overlays;
      });
    in
    {
      overlays = {
        purescript = inputs.purescript-overlay.overlays.default;
        slimlock = inputs.purescript-overlay.inputs.slimlock.overlays.default;
      };

      packages = forAllSystems (system:
        let pkgs = nixpkgsFor.${system}; in
        { default = pkgs.callPackage ./spago.nix { }; }
      );

      devShells = forAllSystems (system:
        let pkgs = nixpkgsFor.${system}; in {
          default = pkgs.mkShell {
            name = "spago";
            buildInputs = with pkgs; [
              purs
              purs-tidy
              purs-backend-es
              spago-unstable

              nodejs
              esbuild
            ];
          };

          dev = pkgs.mkShell {
            shellHook = ''
              source ${self.packages.${system}.default}/share/bash-completion/completions/spago
              
              spago --help

              printf "\nspago version:\n"
              spago --version
            '';
            buildInputs = with pkgs; [
              self.packages.${system}.default
              purs-unstable
            ];
          };
        });
    };
}
