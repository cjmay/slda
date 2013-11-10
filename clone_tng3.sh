#!/bin/bash

set -e

if [ $# -ne 1 ]
then
    echo 'Specify the repo grandparent dir.' >&2
    exit 1
fi

params_name_stem=PfParams
params_rel_path=src/main/scala/lda/RunLda.scala

new_repo_grandparent_dir="$1"
shift

for params_name_prefix in Diff3 Rel3 Sim3
do
    new_repo_parent_dir="$new_repo_grandparent_dir/"`echo "$params_name_prefix" | tr A-Z a-z`
    new_repo_path=`bash clone.sh "$new_repo_parent_dir"`
    echo "$new_repo_path"
    sed -i '/^[[:space:]]*val params: RunLdaParams =.*$/s/=.*$/'"= ${params_name_prefix}${params_name_stem}/" "$new_repo_path/$params_rel_path"
    pushd "$new_repo_path" >/dev/null
    if [ $# -gt 0 ]
    then
        "$@"
    fi
    popd >/dev/null
done
