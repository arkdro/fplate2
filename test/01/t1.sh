#!/bin/sh
# test a number of iterations to fill a plate

iter(){
	local w=$1
	local h=$2
	$prog -t -w $w -h $h
}

batch(){
	local w=$1
	local h=$2
	local base=$3
	for i in `seq -w 0 99`
	do
		fn=${base}-$i
		iter $w $h > $fn
		tail -n 10 $fn | sort -u | egrep '^i=' | awk -F ';' '{print $1}' |\
			awk '{print $1}' | awk -F '=' '{print $2}'
	done
}

do_size(){
	local beg=$1
	local end=$2
	local base=$3
	for s in `seq -w $beg $end`
	do
		w=$s
		h=$s
		fn=${base}-$s
		export width=$w
		echo -n "do_size w=$w, h=$h, avg_cnt="
		batch $w $h $fn | awk '{s+=$1; cnt++}END{e=ENVIRON["width"]; l=log(e); el=e*l; av=(s/cnt); print av, el, av/el}'
	done
}

prog=../../src/rpl
f=`basename $0 .sh`
do_size 3 10 ${f}-res
