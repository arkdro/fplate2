#!/bin/sh
# test of different geometries for psz=2,4,8. Does 20 loops for every psz.
one_loop(){
	local w=$1
	local h=$2
	local psz=$3
	date
	d1=`date "+%s"`
	$PROG -w $w -h $h -psz $psz -ctype 8 -v 0 -loops 20 -ct
	d2=`date "+%s"`
	delta=$(($d2-$d1))
	echo "--- one_loop end, d1=$d1, d2=$d2, delta=$delta"
}

PROG=./rpl.native

echo '1000 1000
800 800
700 700
600 600
500 500
400 400
300 300
250 250
200 200
150 150
100 100
75 75
50 50
25 25' |\
while read w h
do
	echo "w=$w, h=$h"
	one_loop $w $h 2
	one_loop $w $h 4
	one_loop $w $h 8
	echo "=== end"
done
