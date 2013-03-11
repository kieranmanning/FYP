#!/bin/sh

happy LangGrammar.y
rc=$?
if [[ $rc == 0 ]] ; then
    ghc LangGrammar.hs
fi

touch test.hi
rm *.hi
touch test.o
rm *.o

