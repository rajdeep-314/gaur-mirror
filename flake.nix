{
  description = "Gaur - An assembly to VM toolchain for educational purposes";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        ocamlPackages = pkgs.ocamlPackages;
        rocqPackages = pkgs.rocqPackages;

        # OCaml and C libraries required for building and running Gaur
        buildInputs = [
          ocamlPackages.fmt
          ocamlPackages.menhir
          ocamlPackages.num
          ocamlPackages.ocamlgraph
          ocamlPackages.js_of_ocaml
          ocamlPackages.js_of_ocaml-ppx
          rocqPackages.rocq-core
          rocqPackages.stdlib
          pkgs.gmp
        ];

        nativeBuildInputs = [
          ocamlPackages.ocaml
          ocamlPackages.dune_3
          ocamlPackages.findlib
          ocamlPackages.menhir
          ocamlPackages.js_of_ocaml-compiler
          rocqPackages.rocq-core
          pkgs.pkg-config
        ];

        # Precompiled suite of all Gaur binaries
        gaur = pkgs.stdenv.mkDerivation {
          pname = "gaur";
          version = "0.1.0";
          src = self;

          inherit nativeBuildInputs buildInputs;

          preBuild = ''
            export OCAMLPATH="${rocqPackages.rocq-core}/lib:${rocqPackages.rocq-core}/lib/ocaml/4.14.4/site-lib:$OCAMLPATH"
          '';

          buildPhase = ''
            runHook preBuild
            dune build @install
            runHook postBuild
          '';

          checkPhase = ''
            runHook preCheck
            dune runtest
            runHook postCheck
          '';

          installPhase = ''
            runHook preInstall
            dune install --prefix $out assembler disassembler cpu-simulator ram-init vm
            runHook postInstall
          '';

          doCheck = false;
        };

        # Precompiled web tools artifact (HTML + JS bundle)
        web = pkgs.stdenv.mkDerivation {
          pname = "gaur-web";
          version = "0.1.0";
          src = self;

          inherit nativeBuildInputs buildInputs;

          preBuild = ''
            export OCAMLPATH="${rocqPackages.rocq-core}/lib:${rocqPackages.rocq-core}/lib/ocaml/4.14.4/site-lib:$OCAMLPATH"
          '';

          buildPhase = ''
            runHook preBuild
            dune build web/
            runHook postBuild
          '';

          installPhase = ''
            runHook preInstall
            mkdir -p $out/tools
            cp -r _build/default/web/* $out/tools/ 2>/dev/null || true
            cp -r web/pages/* $out/tools/
            runHook postInstall
          '';
        };

      in
      {
        packages = {
          default = gaur;
          gaur = gaur;
          web = web;
        };

        apps = {
          default = flake-utils.lib.mkApp { drv = gaur; name = "assembler"; };
          assembler = flake-utils.lib.mkApp { drv = gaur; name = "assembler"; };
          dis = flake-utils.lib.mkApp { drv = gaur; name = "dis"; };
          disassembler = flake-utils.lib.mkApp { drv = gaur; name = "dis"; };
          cpusim = flake-utils.lib.mkApp { drv = gaur; name = "cpusim"; };
          ram-init = flake-utils.lib.mkApp { drv = gaur; name = "ram-init"; };
          vme = flake-utils.lib.mkApp { drv = gaur; name = "vme"; };
        };

        devShells.default = pkgs.mkShell {
          inputsFrom = [ gaur ];
          packages = [
            ocamlPackages.ocaml-lsp
            ocamlPackages.ocamlformat
            ocamlPackages.odoc
          ];
          shellHook = ''
            export OCAMLPATH="${rocqPackages.rocq-core}/lib:${rocqPackages.rocq-core}/lib/ocaml/4.14.4/site-lib:$OCAMLPATH"
          '';
        };
      }
    );
}
