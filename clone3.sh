#!/bin/bash

set -e

params_name_stem=PfParams
params_rel_path=src/main/scala/lda/RunLda.scala
for params_name_prefix in Diff3 Rel3 Sim3
do
    new_repo_path=`bash clone.sh`
    echo "$new_repo_path"
    gsed -i '/^[[:space:]]*val params: RunLdaParams =.*$/s/=.*$/'"= ${params_name_prefix}${params_name_stem}/" "$new_repo_path/$params_rel_path"
    pushd "$new_repo_path" >/dev/null
    if [ $# -gt 0 ]
    then
        "$@"
    fi
    popd >/dev/null
done
