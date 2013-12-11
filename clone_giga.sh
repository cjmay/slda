#!/bin/bash

set -e

ROOT_REPO_PATH="$PWD"

if [ $# -ne 1 ]
then
    echo "Specify a repo path." >&2
    exit 1
fi

mkdir -p "$1"
new_repo_path="$1"

git clone -q "$ROOT_REPO_PATH" "$new_repo_path"
mkdir "$new_repo_path/results"

echo "$new_repo_path"

export JAVA_HOME="/export/apps/java/bin/java"
export PATH="/export/apps/sbt-0.12.1/bin:$PATH"
export PATH="/export/apps/scala-2.9.2/bin:$PATH"

pushd "$new_repo_path" >/dev/null
sbt package
qsub -q text.q gigaword-qsub-job
popd >/dev/null
