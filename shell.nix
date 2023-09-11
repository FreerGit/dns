let
  nixpkgs-sources =
    builtins.fetchTarball
      https://github.com/nix-ocaml/nix-overlays/archive/master.tar.gz;
  pkgs = import nixpkgs-sources { };
  ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_0;

in
pkgs.mkShell {
  # build tools
  
  nativeBuildInputs = with pkgs; [ curl dune-release ocamlformat inotify-tools ];
  # dependencies
  buildInputs = 
    with ocamlPackages;
     [
      ocaml
      dune_3
      ocaml-lsp

      findlib
      merlin
      utop
      core

      eio
      eio_main
      cstruct
      ipaddr
      ppx_cstruct
      ppx_deriving
      ppx_expect
  ];
}