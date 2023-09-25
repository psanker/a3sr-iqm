#!/usr/bin/env bash

if [ -z "$1" ]; then
    echo "Please provide a folder"
    exit 1
fi

hw_dir="$1"
hw_num="${hw_dir: -1}"

zip "hw${hw_num}_patrick_anker.zip" "$hw_dir/written_responses.pdf" "$hw_dir/analysis.R"
