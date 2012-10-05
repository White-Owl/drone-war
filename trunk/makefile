OBJS=scanner.cmo drone.cmo arena.cmo main.cmo
INTERFACES=ast.cmi
LIBS=

all: DroneWar.exe
	./DroneWar.exe test.dt

DroneWar.exe: $(OBJS)
	ocamlc  -o $@ $^

$(OBJS): $(INTERFACES)

%.cmo %.cmi: %.ml
	ocamlc -warn-error A -c $<

%.ml: %.mll
	ocamllex -o $@ $<

%.ml: %.mly
	ocamlyacc $<

%.cmi: %.mli
	ocamlc -c -o $@ $<

clean:
	rm -f *.cmi *.cmo *.exe

redo: clean all