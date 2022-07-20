{
  description = "ntfd a lightweight notification daemon for fancy desktop integrations.";

  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      overlays = [ haskellNix.overlay
        (final: prev: {
          ntfd =
            final.haskell-nix.project' {
              name = "ntfd";
              src = ./.;
              compiler-nix-name = "ghc8107";
            };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      flake = pkgs.ntfd.flake {};
    in flake // {
      packages.default = flake.packages."ntfd:exe:ntfd";
      apps.default = {
        name = "ntfd";
        type = "app";
        program = "${flake.packages."ntfd:exe:ntfd"}/bin/ntfd";
      };
    });
}
