let
  pinnedNix =
    builtins.fetchGit {
      name = "nixpkgs-pinned";
      url = "https://github.com/NixOS/nixpkgs.git";
      rev = "7c3d4d3af8e9319ccd2a74c31cf247b0fcd08bc2";
    };

  nixpkgs = import pinnedNix {};

  easy-ps = import
    (nixpkgs.pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "5926981701ac781f08b02e31e4705e46b799299d";
      sha256 = "sha256-fA0HEajsXLgI+dRJBYysFExELU8o/TgRnrm2UAju6Q0=";
    }) { pkgs = nixpkgs; };

in

with nixpkgs;

mkShell {
  buildInputs = with pkgs; [
    easy-ps.purs-0_15_4
    easy-ps.spago
    easy-ps.psa
    easy-ps.purescript-language-server
    easy-ps.purs-tidy
  ];
}
