OBJS=parser.cmo scanner.cmo drone.cmo arena.cmo main.cmo
INTERFACES=ast.cmi parser.cmi
LIBS=

all: DroneWar.exe
	./DroneWar.exe test.dt

DroneWar.exe: $(OBJS)
	ocamlc  -o $@ $^

$(OBJS): $(INTERFACES)

%.ml: %.mll
	ocamllex -o $@ $<

%.ml %.mli: %.mly
	ocamlyacc $<

%.cmi: %.mli
	ocamlc -c -o $@ $<

%.cmo %.cmi: %.ml
	ocamlc -warn-error A -c $<

clean:
	rm -f *.cmi *.cmo *.exe

redo: clean all