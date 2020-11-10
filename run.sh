#! /bin/bash
if [ "$1" = "DEBUG" ]; then
    ./a.out DEBUG
elif [ "$#" -eq 3 ]; then
    ./a.out | tee /dev/tty | ./ASMC $1 $2 $3
else
    ./a.out | tee /dev/tty | ./ASMC
fi;

