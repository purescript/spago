{
  pkgs ? import ~/projects/fix-github-https-repo-ps/nix/pkgs.nix
}:


with pkgs;
with pkgs.haskell.lib;

((import ./spago.nix { inherit pkgs; }).override {
  overrides = self: super: {
    spago = justStaticExecutables (
      overrideCabal super.spago (
        old: {
          # originally `preConfigure = "hpack"`,
          # but it throws error `hpack: /homeless-shelter: createDirectory: permission denied (Permission denied)`

          # `export HOME="$TMP"; hpack` doesnt work too - throws `spago.cabal was modified manually, please use --force to overwrite.`

          # disable hpack completely
          preConfigure = "";
        }
      )
    );
  };
}).spago
