
mod scanner;
mod parser;
mod emit;
use std::{fs};
use crate::scanner::*;
use crate::parser::*;
use std::io::prelude::*;

/*
    @todo
    
    type evaluation for := and with it type checking for expressions
    struct initializer {a, b}; {.index = a, .value = b}    
    enum with aggregate stuff enum Animal {Dog, Cat}; if a == .Dog {}
    name_struct_default // for default struct values    
    implement multiple passes, handle struct definition order
    
    later:
    native hash map maybe map: <key, value>?

*/


fn main() {
    let s = fs::read_to_string("test.cues").unwrap();
    
    // let s = s.split(' ');
    
    let mut sc = Scanner::new();
    if false {
        check_expressions();
        return;
    }
    let tokens = sc.scan(s);
    // for t in &sc.tokens { println!("{:?}", t);}
    let mut parser = Parser::new(sc.tokens);
    parser.parse();
    // for (_, proc) in &parser.procs { println!("{}", proc.emit(&parser, &proc.scope));}
    let code = parser.emit_code();
    // println!("{}", code);
    {
        let mut file = fs::File::create("./output/emitted.h").unwrap();
        // @todo not necessary but the vs code C analyzer is being annoying
        file.write_all("#include \"backend.h\"\n\n".as_bytes());
        // /file.read_to_string().unwrap().
        file.write_all(code.as_bytes());
    }
    // println!("parser: {}", parser.primary().to_string());
}

fn check_expressions() {
    let mut sc = Scanner::new();
    let inp = "f".to_string();
    // let inp = "a_proc()".to_string();
    let tokens = sc.scan(inp.clone());
    let mut parser = Parser::new(sc.tokens);
    let expr = parser.expression();
    let _s = Scope::default();
    println!("{}\n{}", inp, expr.emit(&parser,&_s));
    return
}