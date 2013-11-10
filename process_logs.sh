#!/bin/bash

experiment_dir="$1"
output_dir="$2"

python process_log.py "$experiment_dir"
experiment_name=`basename "$experiment_dir"`
mkdir -p "$output_dir/$experiment_name"
mv "$experiment_dir"/*.tab "$output_dir/$experiment_name/"
