#!/bin/bash
k=64
ecut=5
v=13
c=13
r=44
tijera=0
cases=(1 2 3 4 5 6 half-slab)
#cases=(1)
ijk=(zzz xxx xyy yyx)
#ijk=(zzz)
for t in ${ijk[@]}
do
    for i in ${cases[@]}
    do
	printf "\tLayer: $i ijk: $t\n"
	echo all_responses.sh -w layer -m $k'_'$i'_'$ecut-nospin -s $tijera -o 1 -v $v -c $c -r $r  -t \'$t\' > natmat
	chmod +x natmat
	natmat
	rm natmat
    done
done
