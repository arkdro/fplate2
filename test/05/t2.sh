#!/bin/sh
one_loop(){
	local n=$1
	local w=$2
	local h=$3
	for i in `seq 1 $n`
	do
		$PROG -w $w -h $h -psz 4 -ct 8 > /dev/null
	done
}

PROG=./rpl.native

echo '1 1000 1000
2 707 707
3 577 577
4 500 500
5 447 447
6 408 408
7 378 378
8 353 354
9 333 334
10 316 316
11 301 302
12 288 289
13 277 277
14 267 267
15 258 258
16 250 250
17 242 243
18 235 236
19 229 229
20 223 224
25 200 200
30 182 183
35 169 169
40 158 158' |\
while read n w h
do
	echo "n=$n, w=$w, h=$h"
	date
	#for i in `seq 1 $n`;do echo $i ./rpl.native -w $w -h $h -psz 4 -ct 8;done
	one_loop $n $w $h
	date
	echo ====
done
