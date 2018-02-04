FROM ocaml/opam:debian-stable_ocaml-4.04.2

RUN eval `opam config env`
RUN opam update

RUN opam install -y core
RUN opam install -y ppx_deriving
RUN opam install -y menhir

WORKDIR /src

COPY ./bin /src/bin
COPY ./lib /src/lib
COPY ./2bash.opam /src/2bash.opam
