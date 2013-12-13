#!/bin/bash

if [ $# -ne 2 ]
then
    echo 'Specify the experiment dir and tab output dir.' >&2
    exit 1
fi

experiment_dir="$1"
output_dir="$2"

python extract_log_stats.py "$experiment_dir"
experiment_name=`basename "$experiment_dir"`
mkdir -p "$output_dir/$experiment_name"
mv "$experiment_dir"/*.tab "$output_dir/$experiment_name/"
