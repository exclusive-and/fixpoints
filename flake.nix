
{
  description = "Fixpoints of Functors in Haskell";

  inputs = {
    nixpkgs.url   = "nixpkgs/nixos-unstable";
    hasklang.url  = "github:haskell/haskell-language-server/783905f211ac63edf982dd1889c671653327e441";
  };

  outputs = { self, nixpkgs, hasklang }:
    let
      system = "x86_64-linux";

      pkgs = import nixpkgs { inherit system; };

      ghc = pkgs.haskell.packages.ghc96;
    in
    {
      packages.${system}.default = ghc.callCabal2nix "fixpoints" ./. {};

      devShells.${system}.default = pkgs.mkShell {
        packages = [ hasklang.packages.${system}.haskell-language-server-961 ];
      };
    };
}

