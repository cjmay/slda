#!/bin/bash

set -e

if [ $# -ne 3 ]
then
    echo "Specify the TNG training and testing dirs, and the desired output dir." >&2
    exit 1
fi

tng_train_dir="$1"
shift
tng_test_dir="$1"
shift
tng_output_dir="$1"
shift

tng_train_output_dir="$tng_output_dir/train"
tng_test_output_dir="$tng_output_dir/test"

mkdir -p "$tng_train_output_dir"
mkdir -p "$tng_test_output_dir"

max_filename_len=0
for f in "$tng_train_dir"/*/* "$tng_test_dir"/*/*
do
    bn=`basename "$f"`
    len=${#bn}
    if [ $len -gt $max_filename_len ]
    then
        max_filename_len=$len
    fi
done

for f in "$tng_train_dir"/*/*
do
    bn=`basename "$f"`
    while [ ${#bn} -lt $max_filename_len ]
    do
        bn="0$bn"
    done
    cp "$f" "$tng_train_output_dir/$bn"
done

for f in "$tng_test_dir"/*/*
do
    bn=`basename "$f"`
    while [ ${#bn} -lt $max_filename_len ]
    do
        bn="0$bn"
    done
    cp "$f" "$tng_test_output_dir/$bn"
done
