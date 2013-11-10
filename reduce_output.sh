#!/bin/bash

set -e

repo_path="$1"
log_stem="slda.o"

cd "$repo_path"
preserve_dir=".preserve"

mkdir "$preserve_dir"
mv results/results.txt.gz "$preserve_dir/"
git rev-parse HEAD > "$preserve_dir/HEAD"
for f in "$log_stem"*
do
    mv "$f" "$preserve_dir/"
done

rm -rf .git .gitignore *
mv "$preserve_dir/"* ./
rmdir "$preserve_dir"
