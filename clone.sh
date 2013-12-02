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
ln -s "$ROOT_REPO_PATH/data/20news-bydate-train" "$new_repo_path/data/"
ln -s "$ROOT_REPO_PATH/data/20news-bydate-test" "$new_repo_path/data/"
mkdir "$new_repo_path/results"

echo "$new_repo_path"
