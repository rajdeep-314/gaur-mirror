FROM ocaml/opam:debian-12-ocaml-5.2


USER root
RUN apt-get update && apt-get install -y libgmp-dev pkg-config linux-libc-dev vim-common

USER opam
RUN opam update && \
    opam install -y dune menhir num ocamlgraph odoc && \
    opam pin add -y rocq-prover 9.0.0

ENTRYPOINT [ "opam", "exec", "--" ]
CMD [ "/bin/bash" ]

