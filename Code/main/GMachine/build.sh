#!/bin/bash

files=( GCompiler.hs GEval.hs GPrelude.hs GDisplay.hs )

#timeDelay=3;

#while true
#do
#    for file in "${files[@]}"
#    do
#        sleep $timeDelay;
#        ghc $file
#    done
#done

while true 
do 
	EVENT=$(inotifywait -q -e modify --format %f "${files[@]}")
	for file in "${files[@]}"
	do
		ghc $file 
	done
    # remove this when not just ghci'ing
    rm *.hi
    rm *.o
done
# etc... 
