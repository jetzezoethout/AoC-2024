#!/bin/bash

cabal build

for n in $(seq -f "%02g" 1 25)
do
    if [ -d day$n ]; then
        echo "--- Day $n ---"
        executable=$(cabal list-bin day$n)
        inputfile="day$n/input.txt"
        $executable $inputfile
        echo -e "\n"
    fi
done
