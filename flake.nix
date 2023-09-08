{
  description = "Haskell Devenv";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    devenv.url = "github:cachix/devenv";
    nix2container.url = "github:nlewo/nix2container";
    nix2container.inputs.nixpkgs.follows = "nixpkgs";
    mk-shell-bin.url = "github:rrbutani/nix-mk-shell-bin";
  };

  nixConfig = {
    extra-trusted-public-keys =
      "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw=";
    extra-substituters = "https://devenv.cachix.org";
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [ inputs.devenv.flakeModule ];
      systems = [ "x86_64-linux" "aarch64-linux" ];

      perSystem = { config, self', inputs', pkgs, system, ... }:
        let hpkgs = pkgs.haskellPackages;
        in {
          devenv = {
            shells.default = {
              name = "shroomz";

              imports = [
                # This is just like the imports in devenv.nix.
                # See https://devenv.sh/guides/using-with-flake-parts/#import-a-devenv-module
                # ./devenv-foo.nix
              ];

              # https://devenv.sh/reference/options/
              packages = [
                hpkgs.cabal-fmt
                hpkgs.hlint
                hpkgs.fourmolu_0_13_1_0
                pkgs.just
              ];

              languages.haskell = {
                enable = true;
                package = pkgs.haskell.compiler.ghc96;
              };
            };
          };

        };
      flake = { };
    };
}
