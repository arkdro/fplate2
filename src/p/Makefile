OCC = ocamlc
OCI = ocamlc
OCL = ocamlc
OCCFLAGS = -c -annot
OCIFLAGS = -i -annot
OCLFLAGS = -a -annot
PIQI_PATH = $(HOME)/util/ocaml/dist/bin
PIQI_LIB_PATH = $(HOME)/util/ocaml/dist/piqi
PIQI_LIB = piqirun.cma
PROG = plate_piqi.cma
PIQI_FILES = plate.piqi

PIQIC = $(PIQI_PATH)/piqic
PIQIC_FLAGS = --pp

SRC_FILES =

PIQI_ML_FILES = $(patsubst %.piqi, %_piqi.ml, $(PIQI_FILES))
PIQI_ML_EXT_FILES = $(patsubst %.piqi, %_piqi_ext.ml, $(PIQI_FILES))

ML_FILES = \
	$(PIQI_ML_FILES) \
	$(SRC_FILES) \


OBJ_FILES = $(patsubst %.ml, %.cmo, $(ML_FILES))
CMI_FILES = $(patsubst %.ml, %.cmi, $(ML_FILES))

$(PROG): $(OBJ_FILES)
	$(OCL) $(OCLFLAGS) -I $(PIQI_LIB_PATH) $(PIQI_LIB) -o $(PROG) $(OBJ_FILES)

all: $(PROG)

$(PIQI_ML_FILES): $(PIQI_FILES)
	set -e; \
	for i in $^; do \
		$(PIQIC) ocaml-ext $(PIQIC_FLAGS) $$i ; \
	done

$(OBJ_FILES): $(ML_FILES)
	set -e; \
	for i in $^ ; do \
		$(OCC) $(OCCFLAGS) -I $(PIQI_LIB_PATH) $$i \
		; \
	done

mli: $(ML_FILES)
	@set -e; \
	for i in $^ ; do \
		$(OCI) $(OCIFLAGS) -I $(PIQI_LIB_PATH) $$i \
		; \
	done

clean:
	rm -f -- *.tmp.ml *.cmx *.cma *.o $(OBJ_FILES) $(CMI_FILES)

distclean: clean
	rm -f -- $(PIQI_ML_FILES) $(PIQI_ML_EXT_FILES)

tags:
	etags -R .

.PHONY: clean distclean mli tags
