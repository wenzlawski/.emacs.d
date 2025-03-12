{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    naersk.url = "github:nix-community/naersk";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    fenix.url = "github:nix-community/fenix";
  };

  outputs = {
    self,
    flake-utils,
    naersk,
    nixpkgs,
    fenix,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = (import nixpkgs) {
          inherit system;
          overlays = [fenix.overlays.default];
          config.allowUnfree = true;
        };

        naersk' = pkgs.callPackage naersk {};
      in rec {
        defaultPackage = naersk'.buildPackage {
          src = ./.;
          buildInputs = with pkgs; [
            unixODBC
            unixODBCDrivers.msodbcsql18
          ];
        };

        devShell = pkgs.mkShell {
          shellHook = ''
            export LD_LIBRARY_PATH="${pkgs.unixODBC}/lib"

            export ODBCSYSINI=$(realpath ./)

            echo "[ODBC Driver 18 for SQL Server]" > ./odbcinst.ini
            echo "Description = ODBC Driver 18 for SQL Server" >> ./odbcinst.ini
            echo "Driver = ${pkgs.unixODBCDrivers.msodbcsql18}/lib/libmsodbcsql-18.1.so.1.1" >> ./odbcinst.ini
          '';
          nativeBuildInputs = with pkgs; [
            alejandra
            rust-analyzer
            unixODBC
            unixODBCDrivers.msodbcsql18
            (pkgs.fenix.stable.withComponents [
              "cargo"
              "clippy"
              "rust-src"
              "rustc"
              "rustfmt"
            ])
          ];
        };
      }
    );
}
