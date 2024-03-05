#!/bin/bash

cargo run

if [ $? -eq 0 ]; then
    cd output
    ./run.sh
else
    echo "Rust failed."
fi