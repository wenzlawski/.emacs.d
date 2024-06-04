with import <nixpkgs> {};

let
  unstable = import <nixpkgs> { config = config; };
in
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    ffmpeg
  ];
}

