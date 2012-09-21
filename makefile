obj=drone.cmo arena.cmo main.cmo

all: DroneWar.exe
	./DroneWar.exe drones/rabbit.dt drones/turret.dt

DroneWar.exe: $(obj)
	ocamlc -I +/site-lib/extlib extlib.cma -o $@ $^

%.cmo: %.ml
	ocamlc -I +/site-lib/extlib -I +. extlib.cma -c $<

clean:
	rm -f *.cmi *.cmo *.exe