case "$OCAML_VERSION,$OPAM_VERSION" in
e.12.1,1.1.0) ppa=avsm/ocaml312+opam11 ;;
4.00.1,1.1.0) ppa=avsm/ocaml40+opam11 ;;
4.01.0,1.1.0) ppa=avsm/ocaml41+opam11 ;;
*) echo Unknown $OCAML_VERSION,$OPAM_VERSION; exit 1 ;;
esac

echo "yes" | sudo add-apt-repository ppa:$ppa
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam
export OPAMYES=1
export OPAMVERBOSE=1

opam init
opam install ocamlfind cstruct
eval `opam config env`
make
make unix-install
make xen-install
make test
