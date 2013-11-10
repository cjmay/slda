#!/bin/bash

set -e

if [ $# -ne 1 ]
then
    echo 'Specify the experiment dir.' >&2
    exit 1
fi

experiment_dir="$1"
for dataset_dir in "$experiment_dir/"*
do
    for run_dir in "$dataset_dir/"*
    do
        echo "$run_dir"
        bash reduce_output.sh "$run_dir"
    done
done
