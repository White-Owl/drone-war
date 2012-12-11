#!/bin/sh
for f in $(ls test-cases-dbt/*.dbt)
do
	echo $f
	./DroneWar -D $f >/dev/null
	diff -q $f.decompiled $f.etalon
done
