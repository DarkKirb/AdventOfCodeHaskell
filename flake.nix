{
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs;
    flake-utils.url = github:numtide/flake-utils;
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system: let
      pkgs = import nixpkgs {inherit system;};
      haskellPackages = pkgs.haskell.packages.ghc902.override {
        overrides = self: super: {
          ListLike = super.ListLike.overrideAttrs (_: {
            doCheck = false;
          });
        };
      };
    in rec {
      formatter = pkgs.alejandra;
      packages = {
        advent-of-code = haskellPackages.callCabal2nix "AdventOfCode" ./. {};
        default = packages.advent-of-code;
      };
      devShells.default = haskellPackages.shellFor {
        packages = pkgs: [packages.advent-of-code];

        withHoogle = true;

        buildInputs = [
          pkgs.haskell.compiler.ghc902
          haskellPackages.cabal-install
          haskellPackages.cabal2nix
          haskellPackages.haskell-language-server
        ];
      };
      hydraJobs = {
        inherit (packages) advent-of-code;
        devShell = devShells.default;
      };
    });
}
