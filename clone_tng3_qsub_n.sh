#!/bin/bash

if [ $# -eq 2 ]
then
    n=$2
else
    n=30
fi

for i in `seq 1 $n`
do
    bash clone_tng3.sh "$1" qsub -q text.q coe-qsub-job
done
