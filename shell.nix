let
 pkgs = import <nixpkgs> {};
 ocamlPackages = pkgs.ocaml-ng.ocamlPackages_latest;
in
pkgs.mkShell {
  # build tools
  nativeBuildInputs = with pkgs; [ curl git dune-release inotify-tools ];
  # dependencies
  buildInputs = with ocamlPackages; [ ocaml findlib merlin ocamlformat utop core ];
}