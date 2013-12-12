#
# Mettre la liste des fichiers .ml, .mly, .mll et .mli
# constituant le projet. Si un fichier b.ml dépend d'un fichier
# a.ml, a.ml doit se trouver avant dans la liste.
#

SOURCES = ast.mli parser.mly lexer.mll typing.ml mips.ml mips.mli rename.ml compile.mli compile.ml main.ml 

PROJET = le_torriellec-el_sibaie

# Nom du binaire

RAPPORT=rapport

EXEC = minic

#######################################################################
# Partie générique, ne pas modifier.
#######################################################################


# Compilateurs

CAMLC = ocamlc -annot
CAMLOPT = ocamlopt 
CAMLDEP = ocamldep
CAMLLEX = ocamllex
CAMLYACC = ocamlyacc

# Sources

SRC_MLL = $(filter %.mll, $(SOURCES))
SRC_MLY = $(filter %.mly, $(SOURCES))
SMLIY = $(SOURCES:.mly=.ml)
SMLIYL = $(SMLIY:.mll=.ml)
SMLYL = $(filter %.ml,$(SMLIYL))
OBJS = $(SMLYL:.ml=.cmo)
OPTOBJS = $(OBJS:.cmo=.cmx)


all: depend $(EXEC)

opt: depend $(EXEC).opt


$(EXEC): $(OBJS)
	$(CAMLC) $(CUSTOM) -o $(EXEC) $(OBJS)

$(EXEC).opt: $(OPTOBJS)
	$(CAMLOPT) -o $(EXEC) $(OPTOBJS)

.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly

.ml.cmo:
	$(CAMLC) -c $<

.mli.cmi:
	$(CAMLC) -c $<

.ml.cmx:
	$(CAMLOPT) -c $<

.mll.cmo:
	$(CAMLLEX) $<
	$(CAMLC) -c $*.ml

.mll.cmx:
	$(CAMLLEX) $<
	$(CAMLOPT) -c $*.ml

.mly.cmo:
	$(CAMLYACC) $<
	$(CAMLC) -c $*.mli
	$(CAMLC) -c $*.ml

.mly.cmx:
	$(CAMLYACC) $<
	$(CAMLOPT) -c $*.mli
	$(CAMLOPT) -c $*.ml

.mly.cmi:
	$(CAMLYACC) $<
	$(CAMLC) -c $*.mli

.mll.ml:
	$(CAMLLEX) $<

.mly.ml:
	$(CAMLYACC) $<

clean:
	rm -f *.cm[iox] *~ .*~ *.o
	rm -f $(SRC_MLL:.mll=.ml) $(SRC_MLY:.mly=.ml) $(SRC_MLY:.mly=.mli)
	rm -f $(EXEC)
	rm -f $(EXEC).opt

tar: rapport
	mkdir -p $(PROJET)
	cp -r $(SOURCES) Makefile test README $(RAPPORT)/$(RAPPORT).pdf $(PROJET)
	tar czvf $(PROJET).tgz $(PROJET)
	rm -rf $(PROJET)

check: all
	@cd test && ./test.sh


depend: $(SMLIY)
	$(CAMLDEP) $(SMLIY) $(SMLIY:.mly:.mli) > depend

-include depend
