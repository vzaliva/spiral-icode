# Coq sources
COQDIR = coq
COQLIBDIR = ../lib

# OCaml sources
MLDIR = ml
EXTRACTDIR = ml/extracted

COQINCLUDES=$(foreach d, $(COQDIR), -R $(d) Icode) -R $(EXTRACTDIR) Extract

COQC="$(COQBIN)coqc" -q $(COQINCLUDES) $(COQCOPTS)
COQDEP="$(COQBIN)coqdep" $(COQINCLUDES)
COQEXEC="$(COQBIN)coqtop" -q -w none $(COQINCLUDES) -batch -load-vernac-source
MENHIR=menhir
CP=cp

COQFILES := IAst ITypes
OLLVMFILES := 

VFILES := $(COQFILES:%=coq/%.v)
VOFILES := $(COQFILES:%=coq/%.vo)

EXE=icodec

.PHONY: clean test run print-includes print-ocaml-includes coq extracted depend

all:
	@test -f .depend || $(MAKE) depend
	$(MAKE) coq
	$(MAKE) extracted
	$(MAKE) $(EXE)

coq: $(VOFILES)

TSTAMP = $(EXTRACTDIR)/.timestamp

extracted: $(TSTAMP)

$(TSTAMP): $(VOFILES) $(EXTRACTDIR)/Extract.v
	@echo "Extracting"
	rm -f $(EXTRACTDIR)/*.ml $(EXTRACTDIR)/*.mli
	$(COQEXEC) $(EXTRACTDIR)/Extract.v
	touch $(TSTAMP)

%.vo: %.v
	@rm -f doc/$(*F).glob
	@echo "COQC $*.v"
	@$(COQC) -dump-glob doc/$(*F).glob $*.v

depend: $(VFILES) 
	@echo "Analyzing Coq dependencies"
	@$(COQDEP) $^ > .depend

# Directories containing plain Caml code
OCAMLDIRS= $(EXTRACTDIR) $(MLDIR) 

COMMA=,
OCAMLINCLUDES=$(patsubst %,-I %, $(OCAMLDIRS))

print-ocaml-includes:
	@echo $(OCAMLINCLUDES)

OCAMLPACKAGES := core,stdint,ppx_jane,sexplib,ppx_sexp_conv,ppx_compare,ppx_enumerate,getopt

main.native: ml/*.ml ml/extracted/*.ml
	@echo "Compiling $(EXE)"
	ocamlbuild -tags bin_annot,annot,thread -r -use-menhir -yaccflag --explain $(OCAMLINCLUDES) -use-ocamlfind -pkg $(OCAMLPACKAGES) main.native

$(EXE): main.native
	cp main.native $(EXE)

# these are supposed to work only on floats
SCRIPTSF = dft16.g dft48.g dft16-SSE_4x32f.g dft32-SSE_4x32f.g dft48-SSE_4x32f.g dft64-SSE_4x32f.g dft128-AVX_4x64f.g dft128-AVX_4x64f.g
# these are supposed to work only on doubles
SCRIPTSD = dwmonitor.g  
SAMPLESDIR = ../samples/

run: $(EXE)
	./$(EXE) --float --64 --debug --stop $(addprefix $(SAMPLESDIR), $(SCRIPTSF))
	./$(EXE) --double --64 --debug --stop $(addprefix $(SAMPLESDIR), $(SCRIPTSD))

print-includes:
	@echo $(COQINCLUDES)

clean:
	rm -f .depend
	rm -f $(VOFILES)
	rm -rf doc/*.glob
	rm -f $(TSTAMP) $(EXTRACTDIR)/*.ml $(EXTRACTDIR)/*.mli
	ocamlbuild -clean
	rm -rf output
	rm -f $(EXE)

-include .depend
