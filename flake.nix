{
  description = "cutesetup — Haskell TUI for nix devshell scaffolding";

  inputs = {
    nixpkgs.url     = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        hsPkgs = pkgs.haskellPackages;

        cutesetup = hsPkgs.developPackage {
          root = ./.;
          withHoogle = false;
        };
      in {
        packages.default = cutesetup;

        devShells.default = hsPkgs.shellFor {
          packages = _: [ cutesetup ];
          buildInputs = with hsPkgs; [
            cabal-install
            haskell-language-server
            hlint
            ormolu
            ghcid
          ];
          withHoogle = true;
        };
      }
    );
}
