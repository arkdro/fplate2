DEBUG = 0
SRC = src
PIQI = $(SRC)/p
DIRS = $(PIQI) $(SRC)
OCAMLPATH += $(HOME)/util/ocaml/dist

all tags clean distclean:
	echo "target: $@"
	make -C $(PIQI) $@
	export OCAMLPATH="$(OCAMLPATH)"; export DEBUG=$(DEBUG); cd $(SRC); omake $@

.PHONY: tags all clean distclean
