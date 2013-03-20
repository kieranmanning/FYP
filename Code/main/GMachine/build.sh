#!/bin/bash

files=( GCompiler.hs GEval.hs GPrelude.hs GDisplay.hs )

timeDelay=3;

while true
do
    for file in "${files[@]}"
    do
        sleep $timeDelay;
        ghc $file
    done
done
