{
  description = "Language server protocol implementation for Amber";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      rust-overlay,
      ...
    }:
    let
      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];
      forAllSystems = nixpkgs.lib.genAttrs systems;
    in
    {
      packages = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          amber-lsp = pkgs.callPackage ./default.nix { };
        in
        {
          inherit amber-lsp;
          default = amber-lsp;
        }
      );

      devShells = forAllSystems (
        system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ rust-overlay.overlays.default ];
          };
          rustToolchain = pkgs.rust-bin.nightly.latest.default;
          cargo = pkgs.writeShellScriptBin "cargo" ''
            if [ "$1" = "+nightly" ]; then
              shift
            fi
            exec ${rustToolchain}/bin/cargo "$@"
          '';
        in
        {
          default = pkgs.mkShell {
            inputsFrom = [ self.packages.${system}.amber-lsp ];

            packages = with pkgs; [
              cargo
              cargo-insta
              clippy
              nodejs
              pre-commit
              python3
              rustToolchain
            ];
            shellHook = ''
              export PATH="${cargo}/bin:$PATH"
            '';
          };
        }
      );
    };
}
