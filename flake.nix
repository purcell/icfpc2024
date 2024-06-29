{
  description = "ICFPC 2024";

  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";

    ocaml-overlay = {
      url = "github:nix-ocaml/nix-overlays";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, ocaml-overlay }@inputs:
    (
      let
        forAllSystems = nixpkgs.lib.genAttrs nixpkgs.lib.platforms.all;
      in
      {
        devShell = forAllSystems
          (system:
            let
              pkgs = import nixpkgs { inherit system; overlays = [ ocaml-overlay.overlays.default ]; };
              ocamlDeps = with pkgs.ocaml-ng.ocamlPackages_latest; [
                ocaml
                ocaml-lsp
                #ocamlformat
                dune
                utop
                ocp-indent
                ocamlgraph
                zarith
                yojson
                ppx_yojson_conv
                angstrom
                vg
                # # qcheck
                # ppxlib
                ppx_deriving
                # # odoc
                # core_kernel
              ];
            in
            pkgs.mkShell {
              buildInputs = ocamlDeps ++ [ pkgs.jq pkgs.curl ];
            }
          );
      }
    );
}
