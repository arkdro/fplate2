SRC = src
PIQI = $(SRC)/p
DIRS = $(PIQI) $(SRC)
OCAMLPATH += $(HOME)/util/ocaml/dist

tags all clean distclean:
	make -C $(PIQI) $<
	export OCAMLPATH="$(OCAMLPATH)" ; cd $(SRC) ; omake $<

.PHONY: tags all clean distclean
