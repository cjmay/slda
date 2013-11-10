#!/bin/bash

set -e

experiment_dir="$1"
for dataset_dir in "$experiment_dir/"*
do
    for run_dir in "$dataset_dir/"*
    do
        bash reduce_output.sh "$run_dir"
    done
done
