use crate::parser::{*};


pub const S64: &str = "s64";
pub const ADD: &str = "add";
pub const SUB: &str = "sub";
pub const MUL: &str = "mul";
pub const DIV: &str = "div";
pub const GE : &str = "ge";
pub const LE : &str = "le";
pub const EQ : &str = "eq";
pub const GEQ : &str = "geq";
pub const LEQ : &str = "leq";

pub const NOT : &str = "!";
pub const NEQ : &str = "neq";
pub const MOD : &str = "mod";

pub const DEREF: &str = "*";

pub const ASSIGN: &str = "=";
pub const SP: &str = " ";
pub const SNL: &str = ";\n";

// See parser.rs for the individual 'emit' functions
impl Parser {
    pub fn emit_code(&mut self) -> String {
        const _MAIN: &str = "_main";
        let mut code = String::new();
        let mut decl = String::new();
        for (_, proc) in &self.procs {
            let scope = &proc.scope;
            
            type IK = InstructionKind;
            let ret_type = proc.rets.args[0].kind.emit();
            let proc_name = {
                if proc.name == "main" {
                    _MAIN
                } else {
                    &proc.name
                }
            };
            let mut pargs = String::new();
            for a in &proc.args.args {
                pargs += &format!("{} {},", a.kind.emit(), a.name);
            }
            // rem last ','
            if pargs.len() > 0 {pargs.pop();}
            
            decl += &format!("{} {}({});\n", ret_type, proc_name, pargs);
            
            let mut depth = 1;
            /* let scope = (*scope).borrow();
            for inst in &scope.instructions {
                code += &inst.emit(&scope, &self);
            } */
            let main_scope = self.main_scope.borrow();
            code += &format!("{} {}({}) {}\n", ret_type, proc_name, pargs, scope.as_ref().borrow().emit(self, &main_scope));
            // code += "}\n\n";
        }
        let mut st = String::new();
        let mut st_decl = String::new();
        let mut type_infos = String::new();
        let mut type_infos_decl = String::new();
        for  s_name in &self.structs_order {
            let s = self.structs.get(s_name).unwrap();
            st_decl += &s.decl();
            st += &s.emit();
            type_infos_decl += &s.emit_type_info_decl();
            type_infos += &s.emit_type_info();
        }
        let f = format!("// declarations\n\n{}\n\n{}\n\n// definitions\n\n{}\n\n// type infos\n{}\n{}\n\n{}", st_decl, decl, st, type_infos_decl, type_infos, code);
        f
    }
}