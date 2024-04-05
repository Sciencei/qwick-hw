{
  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    nixpkgs.url = "nixpkgs/nixos-unstable";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];
      imports = [
        inputs.haskell-flake.flakeModule
      ];
      perSystem = { self', system, lib, config, pkgs, ... }: {
        haskellProjects.default = {

          packages = {
          };

          devShell = {
            tools = hp: {
              ghcid = pkgs.ghcid;
              node = pkgs.nodejs;
            };
            hlsCheck.enable = false;
          };

          autoWire = [ "packages" "apps" "checks" ];
        };

        devShells.default = pkgs.mkShell {
          name = "my-haskell-package custom development shell";
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
          ];
          nativeBuildInputs = with pkgs; [
          ];
        };
      };
    };
}
