#!/usr/bin/env bash

# Usage: cargo run <verilog_path> <top_module_name> <output_dir>

if [ "$#" -ne 3 ]; then
    echo "Usage: get-json-from-verilog.sh <verilog_file_path> <top_module_name> <output_dir>"
    exit 1
fi

verilog_file_path=$1
top_module_name=$2
output_dir=$3

cd churchroad-gator
cargo build
cargo run $verilog_file_path $top_module_name $output_dir