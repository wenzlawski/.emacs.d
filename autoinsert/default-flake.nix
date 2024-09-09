{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = inputs @ {self, ...}:
    inputs.flake-utils.lib.eachDefaultSystem (system: let
      pkgs = inputs.nixpkgs.legacyPackages.${system};

      nativeBuildInputs = with pkgs; [
	$0
      ];

      buildInputs = with pkgs; [];
    in {
      devShells.default = pkgs.mkShell {inherit nativeBuildInputs buildInputs;};

      packages.default = pkgs.stdenv.mkDerivation {
        pname = "template";
        version = "0.0.0";
        src = ./.;

        nativeBuildInputs =
          nativeBuildInputs
          ++ [
          ];
        inherit buildInputs;
      };
    });
}
