#!/bin/bash

set -e

ROOT_REPO_PATH="$PWD"

NEW_REPO_PATH_STEM="$HOME/slda-run"
i=0
new_repo_path="$NEW_REPO_PATH_STEM-$i"
while [ -d "$new_repo_path" ]
do
    i=$(($i + 1))
    new_repo_path="$NEW_REPO_PATH_STEM-$i"
done

echo "Cloning $ROOT_REPO_PATH to $new_repo_path"

git clone "$ROOT_REPO_PATH" "$new_repo_path"
ln -s "$ROOT_REPO_PATH/data/20news-bydate-train" "$new_repo_path/data/"
ln -s "$ROOT_REPO_PATH/data/20news-bydate-test" "$new_repo_path/data/"
mkdir "$new_repo_path/results"