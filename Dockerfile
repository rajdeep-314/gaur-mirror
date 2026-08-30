FROM ocaml/opam:debian-12-ocaml-5.4


USER root
RUN apt-get update && apt-get install -y libgmp-dev pkg-config linux-libc-dev vim-common

# /bin/sh points to dash in the container but it does not support redirection
# via &>, which is something that we need.
# Check out https://gitlab.com/verify-at-iitpkd/focs/ptools/-/work_items/21 for more details.
RUN ln -sf /bin/bash /bin/sh

USER opam
RUN opam update && \
    opam install -y dune menhir num ocamlgraph odoc && \
    opam pin add -y rocq-prover 9.0.0

ENTRYPOINT [ "opam", "exec", "--" ]
CMD [ "/bin/bash" ]

