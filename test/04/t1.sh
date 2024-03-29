#!/bin/sh

one_bunch(){
	local w=$1
	local h=$2
	local psz=$3
	tmp="$DATA_DIR/tmp-$w-$h-$psz"
	res="$DATA_DIR/tmp-$w-$h-$psz.res"
	id_base="$w:$h:$psz"
	for i in `seq -w 1 $BATCH`
	do
		echo "tid=${id_base}:${i}"
		$RPL -w $w -h $h -psz $psz -ctype 8 -ct -loops 1 -v 1
	done > "$tmp"
	$TEST $TEST_PARAMS -i "$tmp" -o "$res"
	rc=$?
	(
		echo "=== bunch start ==="
		date
		echo "w=$w, h=$h, psz=$psz"
		echo "bunch result=$rc"
		echo "--- dat ---"
		cat "$tmp"
		echo "--- res ---"
		cat "$res"
		echo "=== bunch end ==="
	) >> "$DATA_FILE"
	rm -f -- "$tmp" "$res"
}

RPL=./rpl
TEST=./t1.pl
TEST_PARAMS="-e -v 1"
w0=10
h0=10
psz0=10
BATCH=10
DATA_DIR="data"
DATA_FILE="$DATA_DIR/data_file"
mkdir -p "$DATA_DIR"

for w in `seq -w 2 $w0`
do
	for h in `seq -w 2 $h0`
	do
		for psz in `seq -w 2 $psz0`
		do
			one_bunch $w $h $psz
		done
	done
	tmp_file="$DATA_FILE-$w"
	mv "$DATA_FILE" "$tmp_file"
	gzip "$tmp_file"
done
