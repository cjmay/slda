#!/bin/bash

if [ $# -eq 1 ]
then
    n=$1
else
    n=30
fi

for i in `seq 1 $n`
do
    bash clone_tng3.sh qsub -q text.q coe-qsub-job
done
