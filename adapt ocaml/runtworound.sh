#!/bin/bash

# run the two-round algorithm under columns varying from 100 to 1000

bound=2000
while [ $bound -ge 100 ]
do
	./tworound -i "input.txt" -o "output_$bound.txt" -r 1 -cl $bound -rw 5000 --createdb
	echo $bound
	 ((bound=$bound-200))
done

echo Alldone
