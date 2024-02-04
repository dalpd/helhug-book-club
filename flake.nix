{
  description = "Helsinki Haskell User Group book club flake";

  #inputs.nixpkgs.url = "github:NixOS/nixpkgs/haskell-updates";
  inputs.nixpkgs.url = "github:dalpd/nixpkgs/haskell-updates";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hpkgs = pkgs.haskellPackages.override (old: {
          overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: { }))
            (self: super: {
              sockets-and-pipes = self.callCabal2nix "sockets-and-pipes" ./. { };
            });
        });
      in
      {
        packages = { inherit (hpkgs) sockets-and-pipes; };
        # This can be replaced with an entry point once there are more packages.
        defaultPackage = self.packages.${system}.sockets-and-pipes;
        devShell = hpkgs.shellFor {
          packages = h: [ h.sockets-and-pipes ];
          buildInputs = with hpkgs; [
            cabal-install
            dhall
            dhall-lsp-server
            ghcid
            haskell-language-server
            nixfmt
            ormolu
          ];
          # These can be set to true if needed.
          withHoogle = false;
          withHaddocks = false;
        };
      });
}
