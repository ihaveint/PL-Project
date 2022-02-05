#!/bin/bash


for dir in ./"Tests - Students"/*; do
	in_file=`ls -1 "$dir" | grep -E in.*\.txt`
	out_file=`ls -1 "$dir" | grep -E out.*\.txt`
	echo $in_file
	echo $out_file
	in="${dir}/${in_file}"
	out="${dir}/${out_file}"
	echo $file
	cp "$in" "input.py"
	racket ./main.rkt > out.txt
	diff "${out}" out.txt -y

	echo ""
	echo ""
	echo ""
done
