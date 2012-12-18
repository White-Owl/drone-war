TARGET=DroneWar
ML1=ast.ml utils.ml gui.ml
ML2=bullet.ml drone.ml arena.ml main.ml
MLY=parser_dbt.mly parser.mly
MLL=scanner_dbt.mll scanner.mll
LIBS=graphics.cma unix.cma
OBJS=$(ML1:.ml=.cmo) $(MLL:.mll=.cmo) $(MLY:.mly=.cmo) $(ML2:.ml=.cmo)

# uncomment and recompile to see the full parser log
DEBUG=yes

ifeq ($(OS),Windows_NT)
TARGET:=$(TARGET).exe
endif

OCAML_VERSION:= $(shell ocaml -version | sed -e "s/.*version //g" -e "s/\..*//g")
ifeq ($(OCAML_VERSION), 4)
OCAMLDEP_FLAGS=-all -one-line
endif

all: $(TARGET)
ifeq ($(DEBUG), yes)
	export OCAMLRUNPARAM='p' && ./$(TARGET) -D -t drones/Stephen.dt drones/movingshooter.dt -t drones/George.dt drones/GoCenterGoCrazy.dbt drones/rabbit.dt -t drones/Xiang.dt drones/standshooter.dt drones/turret.dt -t drones/Xiaotong.dt drones/nastyshooter.dt drones/movingshooter2.dt -t drones/Shuo.dt drones/nastyshooter2.dt drones/rabbit2.dt drones/qsLampard.dt 2> stderr
else
	./$(TARGET) -D rabbit.dt turret.dbt
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

.depend: $(ML1) $(MLY:.mly=.ml) $(MLL:.mll=.ml) $(ML2) makefile
	ocamldep $(OCAMLDEP_FLAGS) $(ML1) $(MLY:.mly=.ml) $(MLY:.mly=.mli) $(MLL:.mll=.ml) $(MLL:.mll=.mli) $(ML2) | grep -v ".cmx" > .depend

clean:
	rm -f $(OBJS) $(OBJS:.cmo=.cmi) $(TARGET)
	rm -f $(MLY:.mly=.ml) $(MLY:.mly=.mli) $(MLY:.mly=.output)
	rm -f $(MLL:.mll=.ml) $(MLL:.mll=.mli)
	rm -f .depend
	rm -f stdout stderr
	rm -f *.debug *.decompiled drones/*.debug drones/*.decompiled

redo: clean
	make

ifneq ($(MAKECMDGOALS),clean)
-include .depend
endif
