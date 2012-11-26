TARGET=DroneWar
ML=ast.ml bullet.ml drone.ml arena.ml main.ml 
MLY=parser.mly
MLL=scanner.mll
LIBS=
OBJS=$(MLL:.mll=.cmo) $(MLY:.mly=.cmo) $(ML:.ml=.cmo)

# uncomment and recompile to see the full parser log
#DEBUG=yes

ifeq ($(OS),Windows_NT)
TARGET:=$(TARGET).exe
endif

OCAML_VERSION:= $(shell ocaml -version | sed -e "s/.*version //g" -e "s/\..*//g")
ifeq ($(OCAML_VERSION), 4)
OCAMLDEP_FLAGS=-all -one-line
endif

all: $(TARGET)
ifeq ($(DEBUG), yes)
	export OCAMLRUNPARAM='p' && ./$(TARGET) test2.dt  > stdout 2> stderr
else
	./$(TARGET) -D test.dt test2.dt
endif

$(TARGET): $(OBJS)
	ocamlc  -o $@ $(LIBS) $^

%.ml: %.mll
	ocamllex $<

%.ml %.mli: %.mly
ifeq ($(DEBUG), yes)
	ocamlyacc -v $<
else
	ocamlyacc $<
endif

%.cmi: %.mli
	ocamlc -warn-error A -c $<

%.cmo %.cmi: %.ml
	ocamlc -warn-error A -c $<

%.mli: %.ml
	ocamlc -i $< >$@

.depend: $(ML) $(MLY:.mly=.ml) $(MLL:.mll=.ml) makefile
ifeq ($(OS),Windows_NT)
	@attrib -H $@
endif
	ocamldep $(OCAMLDEP_FLAGS) $(ML) $(MLY:.mly=.ml) $(MLY:.mly=.mli) $(MLL:.mll=.ml) $(MLL:.mll=.mli) | grep -v ".cmx" > .depend
ifeq ($(OS),Windows_NT)
	@attrib +H $@
endif

clean:
	rm -f $(OBJS) $(OBJS:.cmo=.cmi) $(TARGET)
	rm -f $(MLY:.mly=.ml) $(MLY:.mly=.mli) $(MLY:.mly=.output)
	rm -f $(MLL:.mll=.ml) $(MLL:.mll=.mli)
	rm -f .depend
	rm -f stdout stderr
	rm -f *.debug *.decompiled

redo: clean
	make

ifneq ($(MAKECMDGOALS),clean)
-include .depend
endif
