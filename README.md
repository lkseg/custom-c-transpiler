- [Project](#Project)

# Project 

This project is a transpiler from a custom language to C written in Rust.

See *./test.cues* and *./output/emitted.h* for the translated C code.

Some current features are
- Automatic type info generation for structs. This also allows for automatically passing the correct type info to a procedure e.g. print(A_Struct) just works
- Go/Zig like defer statement for the scope
- Ability to override the basic behaviour of basic operators like '+' with wrapping and automatic bounds checking for arrays
- Simple and clean syntax. Also no '->' for pointers just '.'
- All objects get 0 initialized.

