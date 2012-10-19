TARGET=DroneWar
ML=ast.ml drone.ml arena.ml main.ml
MLY=parser.mly
MLL=scanner.mll
LIBS=
OBJS=$(MLL:.mll=.cmo) $(MLY:.mly=.cmo) $(ML:.ml=.cmo)


ifeq ($(OS),Windows_NT)
TARGET:=$(TARGET).exe
endif

all: $(TARGET)
	./$(TARGET) test.dt


$(TARGET): $(OBJS)
	ocamlc  -o $@ $(LIBS) $^

%.ml: %.mll
	ocamllex $<

%.ml %.mli: %.mly
	ocamlyacc $<

%.cmi: %.mli
	ocamlc -warn-error A -c $<

%.cmo %.cmi %.mli: %.ml
	ocamlc -warn-error A -c $<

%.mli: %.ml
	ocamlc -i $< >$@


.depend: $(ML) $(MLY:.mly=.ml) $(MLL:.mll=.ml) makefile
ifeq ($(OS),Windows_NT)
	@attrib -H $@
endif
	ocamldep -all $(ML) $(MLY:.mly=.ml) $(MLY:.mly=.mli) $(MLL:.mll=.ml) $(MLL:.mll=.mli) | grep -v ".cmx" > .depend
ifeq ($(OS),Windows_NT)
	@attrib +H $@
endif

clean:
	rm -f $(OBJS) $(OBJS:.cmo=.cmi) $(TARGET)
	rm -f $(MLY:.mly=.ml) $(MLY:.mly=.mli)
	rm -f $(MLL:.mll=.ml) $(MLL:.mll=.mli)
	rm -f .depend

redo: clean
	make

ifneq ($(MAKECMDGOALS),clean)
-include .depend
endif
