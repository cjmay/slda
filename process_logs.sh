#!/bin/bash

log_dir="$1"
output_dir="$2"

for d in "$log_dir"/*
do
    python process_log.py "$d"
    bn=`basename "$d"`
    mkdir -p "$output_dir/$bn"
    mv "$d"/*.tab "$output_dir/$bn/"
done
