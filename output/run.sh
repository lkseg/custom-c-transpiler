#!/bin/bash

EXE_NAME="cues"

SRCS="main.c backend.c"


gcc -std=c17 -I./ -o $EXE_NAME $SRCS

if [ $? -eq 0 ]; then
    echo "Compilation successful"
    echo "Running c program"
    ./cues
else
    echo "Compilation failed."
fi
