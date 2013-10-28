#!/bin/bash

filename="$1"
data_filename=`mktemp timing.XXXXXX`

perl -n -e'/^\d+ \/ \d+\t{3}(\d+)$/ && print "$1\n"' "$filename" > "$data_filename"
R --vanilla << EOF
d <- read.table("$data_filename")
plot(1:dim(d)[1], d[,1])
EOF
rm -f "$data_filename"
