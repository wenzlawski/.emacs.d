{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = inputs@{
    self, ...
  }: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = inputs.nixpkgs.legacyPackages.\${system};
  in {
    devShells.default = pkgs.mkShell {
       nativeBuildInputs = with pkgs; [
          $0
       ];
    };
  });
}
