
{
  description = "Fixpoints of Functors in Haskell";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";

      pkgs = import nixpkgs { inherit system; };

      ghc = pkgs.haskell.packages.ghc96;
    in
    {
      packages.${system}.default = ghc.callCabal2nix "fixpoints" ./. {};
    };
}

