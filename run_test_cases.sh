#!/bin/sh
for f in $(ls test-cases-dbt/*.dbt)
do
	echo $f
	./DroneWar -q -D $f >/dev/null
	rm $f.debug
	diff -q $f.decompiled $f.etalon
done
