#!/bin/bash

set -e

if [ $# -ne 1 ]
then
    echo 'Specify the repo parent dir.' >&2
    exit 1
fi

export JAVA_HOME="/export/apps/java/bin/java"
export PATH="/export/apps/sbt-0.12.1/bin:$PATH"
export PATH="/export/apps/scala-2.9.2/bin:$PATH"

params_name_stem=PfParams
params_rel_path=src/main/scala/lda/RunLda.scala

new_repo_parent_dir="$1"

for params_name_prefix in Diff3 Rel3 Sim3
do
    new_repo_path="$new_repo_parent_dir/"`echo "$params_name_prefix" | tr A-Z a-z`
    bash clone.sh "$new_repo_path"
    sed -i '/^[[:space:]]*val params: RunLdaParams =.*$/s/=.*$/'"= ${params_name_prefix}${params_name_stem}/" "$new_repo_path/$params_rel_path"
    pushd "$new_repo_path" >/dev/null
    sbt package
    qsub -q text.q coe-qsub-job
    popd >/dev/null
done
