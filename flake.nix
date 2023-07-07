
{
  description = "Fixpoints of Functors in Haskell";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      # Overlay to manage dependencies in the Nix Haskell package set.
      haskOverlay = final: prev:
        {
          hask      = final.haskell.packages.ghc96;
          fixpoints = final.hask.callCabal2nix "fixpoints" ./. {};
        };

      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; overlays = [ haskOverlay ]; };
    in
    {
      overlays.default = haskOverlay;

      packages.${system}.default = pkgs.fixpoints;

      # Build a development shell for this package.
      devShells.${system}.default = pkgs.hask.shellFor {
        packages = _: [ pkgs.fixpoints ];

        # Library dependencies.
        buildInputs = [];

        # Add Haskell Language Server and Cabal as development tools.
        nativeBuildInputs = [
          pkgs.hask.cabal-install
          pkgs.hask.haskell-language-server
        ];
      };
    };
}

