#!/bin/bash

set -e

if [ $# -ne 1 ]
then
    echo 'Specify the repo dir.' >&2
    exit 1
fi

repo_path="$1"
log_stem="slda.o"

cd "$repo_path"
preserve_dir=".preserve"

mkdir "$preserve_dir"
gzip results/results.txt.gz
mv results/results.txt.gz "$preserve_dir/"
git rev-parse HEAD > "$preserve_dir/HEAD"
for f in "$log_stem"*
do
    mv "$f" "$preserve_dir/"
done

rm -rf .git .gitignore *
mv "$preserve_dir/"* ./
rmdir "$preserve_dir"
