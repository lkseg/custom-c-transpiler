
use std::borrow::BorrowMut;
use std::{rc::Rc, cell::RefCell, collections::HashMap};

use crate::scanner::*;
use crate::emit;
type TT = TokenType;

#[derive(Clone, Copy, Debug)]
pub enum InstructionKind {
    Error,
    DefAssign,
    Assign,
    Return,
    If,
    Else,
    For,
    While,
    Expression,
    Break,
    Continue,
    Scope,
    Defer,
}
type IK = InstructionKind;
pub struct StructDef {
    name: String,
    vars: Vec<Var>,
}
impl StructDef {
    pub fn emit_type_info_decl(&self) -> String {
        return format!("const Type_Info Type_Info_{};\n", self.name);
    }
    pub fn emit_type_info(&self) -> String {
        let mut infos = String::new();
        let mut offsets = String::new();
        for v in &self.vars {
            infos += &format!("&{}, ", v.kind.get_type_info_name().as_str());
            offsets += &format!("offsetof({}, {}), ", self.name, v.name);
        }
        // infos.pop();
        // offsets.pop();
        
        let mut s = format!("const Type_Info Type_Info_{} = {{ \"{}\", TYPE_STRUCT, NULL, {{
            (const Type_Info *[]){{ {} }},
            (const usize []){{ {} }},
            {}
        }}\n}};\n", self.name, self.name, infos, offsets, self.vars.len());
        
        s
    }
    pub fn decl(&self) -> String {
        return format!("struct {}; typedef struct {} {};\n", self.name, self.name, self.name);
    }
    pub fn emit(&self) -> String {        
        let mut s = String::new();
        for e in &self.vars {
            s += &format!("{} {};\n", e.kind.emit(), e.name);
        }
        return format!("struct {} {{\n{}\n}};\n", self.name, s);
    }
}

// pub type ExpressionUid = u64;
pub struct ExpBinary {
    token: Token,
    left: Rc<dyn Expression>,
    right: Rc<dyn Expression>,
}
impl ExpBinary {
    pub fn new(token: Token, left: Rc<dyn Expression>, right: Rc<dyn Expression>) -> Self {
        return Self{token, left, right};
    }
}
pub struct ExpLiteral {
    literal: Literal,
}
pub struct ExpGroup {
    expression: Rc<dyn Expression>,  
}

// pub struct ExpArg {}

#[derive(Clone)]
pub struct ExpArgs {
    pub args: Vec<Var>,  
}
#[derive(Clone, Default)]
pub struct ExpCall {
    pub name: String,
    pub args: Vec<Rc<dyn Expression>>,  
}



pub struct Procedure {
    pub name: String,
    pub args: ExpArgs,
    pub rets: ExpArgs,
    pub scope: ScopeRef,
}
pub struct ExpUnary {
    token: Token,
    right: Rc<dyn Expression>,
}
impl ExpUnary {
    pub fn new(token: Token, right: Rc<dyn Expression>) -> Self {
        return Self{token, right};
    }
}


#[derive(Clone)]
pub struct LValue {
    expr: Rc<dyn Expression>,
}
impl LValue {
    pub fn emit(&self, p: &Parser, scope: &Scope) -> String {
        return self.expr.emit(p, scope);
    }
}
#[derive(Clone)]
pub struct RValue {
    pub kind: VarKind,
    pub expr: Rc<dyn Expression>,
}
impl RValue {
    pub fn emit(&self, p: &Parser, scope: &Scope) -> String {
        return self.expr.emit(p, &scope);
    }
}

#[derive(Clone)]
pub enum ForKind {
    Array([String; 3]),
    Range(String, Rc<dyn Expression>, Rc<dyn Expression>),
}
#[derive(Clone)]
pub struct ForLoop {
    pub inner: ForKind,
    pub scope: Rc<RefCell<Scope>>,
}
impl ForLoop {
    pub fn emit(&self, p: &Parser, scope: &Scope) -> String {
        if let ForKind::Array(vec) = &self.inner {
            let arr = &vec[2];
            let it = &vec[0];
            let ix = &vec[1];
            let arr_type = scope.variables.get(arr).unwrap().kind.emit();
            // @todo get inner type from array
            return format!("{{\n for(isize {} = 0; {} < array_len({}); ++{}) {{ \n {} {} = &array_get({}, {}); {}\n }}\n}}\n", ix, ix, arr, ix, arr_type, it, arr, ix, self.scope.borrow().emit(p, scope))
        } else if let ForKind::Range(ix, left, right) = &self.inner {
            return format!("{{\n for(isize {ix} = {}; {ix} < {}; ++{ix}) {{ {} }}\n}}\n", left.emit(&p, &scope), right.emit(&p, &scope), self.scope.borrow().emit(p, scope));
        }
        panic!("");
    }
}
#[derive(Clone)]
pub enum RInstruction {
    None,
    RValue(RValue),
    ForLoop(ForLoop),
    Scope(Rc<RefCell<Scope>>),
}
type RI = RInstruction;
impl RInstruction {
    pub fn emit(&self, p: &Parser, scope: &Scope) -> String {
        match self {
            Self::None => "".into(),
            Self::ForLoop(v) => v.emit(p,scope),
            Self::RValue(v) => v.emit(p,scope),
            Self::Scope(inner) => (*inner).borrow().emit(p, scope),
        }
    }
    pub fn is_null(&self) -> bool {
        match self {            
            Self::RValue(v) => v.expr.is_null(),
            _ => false,
        }
    }
}

#[derive(Clone)]
pub struct ExpInstruction {
    pub kind: InstructionKind,
    // pub left_string: String,
    pub left: Option<LValue>,
    pub right: RInstruction, // should be an evaluted Rc<dyn Expression>
}
fn struct_var_name(s: &str) -> (&str, bool) {
    let mut idx: isize = -1;
    // ignore asterisk
    let mut start = 0;
    // let b = s.as_bytes();
    for (i, c) in s.chars().enumerate() {
        if c == '.' || c == '[' {
            idx = i as isize;
            break;
        } else if  c== '*' {
            // @todo
            // pretty much ths whole thing todo
            start += 1;
        }
    }
    if idx < 0 {
        return (&s[start..], false);
    }
    return (&s[start..idx as usize], true)
}
fn _scope_ident(s: &str) -> isize {
    let mut idx: isize = -1;
    // let b = s.as_bytes();
    for (i, c) in s.chars().enumerate() {
        if c == '.' || c == '[' {
            idx = i as isize;
            break;
        }
    }
    return idx;
}
impl ExpInstruction {
    pub fn emit(&self, scope: &Scope, p: &Parser) -> String {
        match self.kind {
            IK::Assign => {
                let left_string = self.left.as_ref().unwrap().emit(p, &scope);
                // struct.member := i // this is not valid code!
                // let var = scope.variables.get(&left_string).unwrap();
                format!("{} {} {};\n", left_string, emit::ASSIGN, &self.right.emit(p, &scope))
            }
            IK::DefAssign => {
                let left_string = self.left.as_ref().unwrap().emit(p, &scope);
                // struct.member := i // this is not valid code!
                let var = scope.variables.get(&left_string).unwrap();
                if self.right.is_null() {
                    // @todo proper default structs
                    format!("{} {} {} {};\n", var.kind.emit(), &var.name, emit::ASSIGN, "{0}")
                } else {
                    format!("{} {} {} {};\n", var.kind.emit(), &var.name, emit::ASSIGN, &self.right.emit(p, &scope))
                }
            }
            IK::Return => {
                let r = self.right.emit(&p, &scope);
                if scope.defers.len() == 0 {
                    format!("return {};\n", r)
                } else {
                    let defers = scope.emit_defers(&p, &scope);
                    format!(";{};\n return {};\n", defers, r)
                }                                
            }
            IK::Expression => {
                // let f: String = if.right
                format!("{};\n", self.right.emit(p, &scope))
            }
            IK::Scope => {
                format!("{}\n", self.right.emit(p, &scope))
            }
            IK::If => {
                format!("if({}) ", self.right.emit(p, &scope))
            }
            IK::While => {
                format!("while({}) ", self.right.emit(p, &scope))
            }
            IK::Else => {
                "else ".into()
            }            
            IK::For => {
                self.right.emit(p, &scope)
            }
            IK::Defer => "".into(),
            IK::Continue => "continue;\n".into(),
            IK::Break => "break;\n".into(),
            _ => {
                unreachable!("can't emit instruction '{:?}'", self.kind);
            }
        }        
    }
}
#[derive(Clone, Debug)]
pub enum VarKind {
    Error,
    Null,    
    S64,
    F64,
    Bool,
    Void,
    String,
    Pointer(Box<VarKind>),
    Array(Box<VarKind>),
    Struct(String),
    Enum,
}
impl VarKind {
    pub fn emit(&self) -> String {
        // @todo use a buffer and return &str
        match self {
            Self::S64 => {
                "s64".into()
            }
            Self::F64 => {
                "f64".into()
            }
            Self::Bool => {
                "bool".into()
            }
            Self::String => {
                "String".into()
            }
            Self::Void => {
                "void".into()
            }
            Self::Pointer(next) => {
                format!("{} *", next.emit())
            }
            Self::Array(inner) => {
                format!{"{} *", inner.emit()} // Same as Pointer because of stb_ds
            }
            Self::Struct(name) => {
                name.into()
            }
            _ => {
                unreachable!();
            }
        }
    }
    // Type_Info_*
    pub fn get_type_info_name(&self) -> String {
        let mut s = "Type_Info_".to_string();
        s += match self {
            VarKind::S64 => "S64".into(),
            VarKind::F64 => "F64".into(),
            VarKind::String => "String".into(),
            VarKind::Struct(name) => &name,
            VarKind::Bool => "Bool".into(), 
            // @todo inner
            VarKind::Pointer(_) => "Pointer".into(), 
            _ => panic!("not implemented for {:?}", self),
        };
        s
    }
    pub fn _basic_from_string(s: &str) -> VarKind {
        match s {
            "int" => VarKind::S64,
            "s64" => VarKind::S64,
            "f64" => VarKind::F64,
            "bool" => VarKind::Bool,
            "void" => VarKind::Void,
            "string" => VarKind::String,
            // @todo check for struct existence at some point
            _ => Self::Struct(s.to_string()),
        }
    }
    pub fn _from_string(s: &str) -> VarKind {
        if s.chars().next().unwrap() == '*' {
            return VarKind::Pointer(Box::new(VarKind::_from_string(&s[1..])));
        }
        Self::_basic_from_string(s)
    }
}
#[derive(Clone)]
pub struct Var {
    pub name: String,
    pub kind: VarKind,
}
impl Var {
    pub fn new(name: String, kind: VarKind) -> Self {
        Self {
            name,
            kind,
        }
    }
}

#[derive(Clone)]
pub struct ExpSubscribe {
    pub left: Rc<dyn Expression>, // left points to the left side of an expression; furthermore in the case of m[x][y] we have {left: m[x], inner: y}
    pub inner: Rc<dyn Expression>, 
}
impl Expression for ExpSubscribe {
    fn emit(&self, p: &Parser, scope: &Scope) -> String {
        format!("array_get({},({}))", self.left.emit(p, scope), self.inner.emit(p, scope))
    }
}
type ScopeRef = Rc<RefCell<Scope>>;
#[derive(Default, Clone)]
pub struct Scope {
    pub parent: Option<ScopeRef>,
    pub instructions: Vec<ExpInstruction>,
    pub variables: HashMap<String, Var>,
    pub defers: Vec<ScopeRef>,
}

impl Scope {
    // fn find_variable
    fn get_variable(&self, name: &str) -> Var {
        let var = self.variables.get(name);
        if var.is_none() {
            if self.parent.is_none() {
                panic!("couldn't find variable {}", name);
            } else {
                let p = self.parent.as_ref().unwrap().borrow();
                return p.get_variable(name).clone();
            }
        }
        return var.unwrap().clone();
    }
    pub fn emit_defers(&self, p: &Parser, scope: &Scope) -> String {
        let mut defers = String::new();
        for i in 0..self.defers.len() {
            defers += &self.defers[i].borrow().emit(p, scope);
        }
        defers
    }
    pub fn emit(&self, p: &Parser, scope: &Scope) -> String {
        let mut s = String::new();
        for i in &self.instructions {
            s += &i.emit(&self, p);
        }
        let defers = self.emit_defers(&p, &scope);
        // println!("{}, {}", self.defers.len(), scope.defers.len());
        if self.defers.len() > 0 {
            format!("{{\n{}; {}}}", s, defers)
        } else {
            format!("{{\n{}}}", s)
        }
    }    
}




// @todo parse_type
pub trait Expression {
    fn is_null(&self) -> bool {false}
    fn emit(&self, p: &Parser, scope: &Scope) -> String {
        return "todo".into();
    }
    fn get_literal(&self) -> &Literal {
        unreachable!("not a literal");
    }
}
struct NullExpression{}
impl Expression for NullExpression {

    fn emit(&self, p: &Parser, scope: &Scope) -> String {"".to_string()}    
    fn is_null(&self) -> bool {true}
}
impl Expression for ExpArgs {}


impl Expression for ExpBinary {
    fn emit(&self, p: &Parser, scope: &Scope) -> String {
        let op = 
        match self.token.kind {
            TT::Plus    => &emit::ADD,
            TT::Minus   => &emit::SUB,
            TT::Star    => &emit::MUL,
            TT::Slash   => &emit::DIV,
            TT::Greater => &emit::GE,
            TT::Less    => &emit::LE,
            TT::Equal   => &emit::EQ,
            TT::GreaterEqual => &emit::GEQ,
            TT::LessEqual    => &emit::LEQ,
            TT::NotEqual     => &emit::NEQ,
            TT::Mod     => &emit::MOD,
            _ => unreachable!("can't emit '{:?}'", self.token.kind),
        };
        // @todo
        let kind = &emit::S64;
        
        // let s = format!("{} {} {}", self.left.emit(), self.token.emit(), self.right.emit());
        let s = format!("{}_{}({}, {})", kind, op, self.left.emit(p, &scope), self.right.emit(p, &scope));
        s
    }
}

impl ExpLiteral {
    fn _self_parse(&self, name: &str, scope: &Scope) -> String {
        let idx = _scope_ident(&name);
        if idx < 0 {
            return self.literal.emit();
        }
        let idx = idx as usize;
        // let name = name.as_str();
        let var = scope.variables.get(&name[0..idx]).unwrap();
        // @todo a.b.c // rn. just a.b
        // for c one must look into the type definition of a to check for the type of b 
        return match var.kind {
            VarKind::Pointer(_) => format!("{}->{}", &name[0..idx], &name[idx+1..]),
            VarKind::Struct(_) => format!("{}.{}", &name[0..idx], &name[idx+1..]),
            _ => panic!("panic for: {:?}", var.kind),
        }
    }
}
impl Expression for ExpLiteral {
    fn emit(&self, p: &Parser, scope: &Scope) -> String {
        if let Literal::Identifier { name } = &self.literal {
            return self._self_parse(&name, &scope)
        }
        return self.literal.emit();        
    }
    fn get_literal(&self) -> &Literal {
        return &self.literal;
    }    
}
impl Expression for ExpGroup {

    fn emit(&self, p: &Parser, scope: &Scope) -> String {
        let s = format!("({})", self.expression.emit(p, scope));
        s
    }    
}
impl Expression for ExpUnary {

    fn emit(&self, p: &Parser, scope: &Scope) -> String {
        
        let s = 
        match self.token.kind {
            TT::Minus => format!("-{}", self.right.emit(p, &scope)),
            TT::Not => format!("!{}", self.right.emit(p, &scope)),
            // @todo
            TT::REF => format!("&{}", self.right.emit(p, &scope)),
            TT::DEREF => format!("*{}", self.right.emit(p, &scope)),
            _ => unreachable!("unknown unary"),
        };
        s
    }    
}
impl Expression for ExpCall {

    fn emit(&self, p: &Parser, scope: &Scope) -> String {
        let mut args = String::new();
        // @todo
        if self.name == "print" {        
            for v in &self.args {
                args += &format!("&{},", v.emit(p, &scope));
            }
        } else {
            for v in &self.args {
                args += &format!("{},", v.emit(p, &scope));
            }            
        }
        if self.args.len() > 0 {
            // remove last ','
            args.pop();
        }
        let s = 
        if self.name == "print" {
            // @todo
            let name = self.args[0].emit(p, &scope);
            let var = scope.get_variable(&name);
            let ti_name = var.kind.get_type_info_name();
            format!("print({}, &{})", args, ti_name)
        } else {
            format!("{}({})", self.name, args)
        };
        s
    }
}
#[derive(Copy, Clone, PartialEq, Eq)]
pub enum ScopeKind {
    File,
    Proc,
}
pub struct Parser {
    pub current: usize,
    pub tokens: Vec<Token>,
    pub scope_kind: ScopeKind,
    pub main_scope: ScopeRef,
    pub procs: HashMap<String, Procedure>,
    pub structs: HashMap<String, StructDef>,
    pub structs_order: Vec<String>, // @todo
    // expressions: Vec<Rc<dyn Expression>>,
}
impl Parser {
    pub fn print() {
        
    }
    
    pub fn new(tokens: Vec<Token>) -> Self {
        return Parser {
            current: 0,
            tokens,
            scope_kind: ScopeKind::File,
            procs: HashMap::new(),
            structs: HashMap::new(),
            structs_order: Default::default(),
            main_scope: Rc::new(RefCell::new(Default::default())),
        }
    }
    fn _parse(&mut self) {
        if self.scope_kind == ScopeKind::File {
            loop {
                self.consume_terminator();                                
                if self.finished() {
                    break;
                } else if self.peek().kind != TT::Identifier {
                    break;
                }
                
                if self.is_next_struct() {
                    self.parse_struct();
                } else {
                    self.procedure(self.main_scope.clone());
                }
            }
        }        
    }
    fn consume_terminator(&mut self) {
        self.peek_token(TokenType::Terminator);
    }
    pub fn parse(&mut self) {
        self._parse();
    }
    pub fn consume_type_string(&mut self) -> String {
        // @todo; just iterate
        if self.peek_token(TT::Star) {
            return format!("*{}", self.consume_type_string());
        }
        return self.consume(TT::Identifier, "todo : [] type without ident").literal.cast_string();
    }
    fn procedure_args(&mut self) -> ExpArgs {
        self.consume(TT::LeftParen, "proc args start with '('");
        let mut args = Vec::<Var>::new();
        // ident: type, ...
        while self.peek().kind == TT::Identifier {  
            let ident = self.consume_next().literal.cast_string();
            self.consume(TT::Define, "':' after proc arg name");
            // let t = self.consume(TT::Identifier, "type name after var: in proc args");
            // let t = self.consume_type_string();
            let t = self.read_type();
            args.push(Var::new(ident, t));
            if !self.peek_token(TT::Comma) {
                break;
            }
        }        
        self.consume(TT::RightParen, "proc args end with ')'");
        return ExpArgs{args};
    }
    fn procedure_call(&mut self) -> ExpCall {        
        let proc_name = match self.peek().kind {
            TT::Identifier => self.consume(TT::Identifier, "identifier for procedure").literal.cast_string(),
            TT::Print => {
                self.advance();
                "print".into()
            }
            _ => unreachable!("unknown way of identifying procedure"),
        };

        self.consume(TT::LeftParen, "procedure call starts with '('");
        if self.peek_token(TT::RightParen) {
            return ExpCall{name: proc_name, args: vec![]};
        }
        let mut args = ExpCall::default();
        loop {
            let p = self.expression();
            args.args.push(p);
            if !self.peek_token(TT::Comma) {
                break;
            }
        }
        args.name = proc_name;
        self.consume(TT::RightParen, "procedure call ends with ')'");
        return args;
    }
    fn procedure_rets(&mut self) -> ExpArgs {
        // self.consume(TT::LeftParen, "proc args start with '('");
        // @todo type; alternative ()        
        let vkind = self.read_type();        
        // self.consume(TT::RightParen, "proc args end with ')'");
        return ExpArgs{args: vec![Var::new("".into(), vkind)]};
    }
    
    fn instruction(&mut self, scope: ScopeRef, proc_rets: Option<&ExpArgs>) -> Option<ExpInstruction> {
 
        let first = self.consume_next().clone();
        
        // let _rval = self.consume(TT::Number, "todo");
        
        let mut lval_string = String::new();
        let mut rval: RInstruction;
        let mut lval: Option<LValue> =  None;
        let mut inst_kind = InstructionKind::Error;
        'match_kind: { match first.kind {
            TT::Identifier | TT::DEREF => {
                self.undo();
                // @note this can be funky IF Assign and stuff will be part of an expression
                let ident = self.expression();                
                // let ident = first;
                let kind = self.consume_next().kind;
                if kind == TT::DefAssign {
                    inst_kind = InstructionKind::DefAssign;
                    let _rval = self.expression();
                    let literal = ident.get_literal().clone();
                    if let Literal::Identifier { name } = literal {
                        lval_string = name.clone();
                        lval = Some(LValue { expr: ident});
                        // @todo
                        let var = Var { name: name.clone(), kind: VarKind::S64};
                        let mut scope = (*scope).borrow_mut();
                        scope.variables.insert(name, var);
                    } else {
                        panic!("lvalue for ':=' must be just an identifier")
                    }
                    // @todo evaluate for type
                    rval = RI::RValue(RValue{kind: VarKind::S64, expr: _rval});
                } else if kind == TT::Define {
                    inst_kind = InstructionKind::DefAssign;
                    

                    let literal = ident.get_literal().clone();
                    // let var_kind = self.consume(TT::Identifier, "expecting type name after :").clone();
                    let var_kind = self.read_type();
                    if let Literal::Identifier { name } = literal {
                        lval_string = name.clone();
                        lval = Some(LValue { expr: ident});
                        let var = Var { name: name.clone(), kind: var_kind.clone()};
                        let mut scope = (*scope).borrow_mut();
                        scope.variables.insert(name, var);
                    } else {
                        panic!("lvalue for ': Type =' must be just an identifier")
                    }
                    
                    // just 'var: type' will be translated to 'var: type = null'
                    if self.peek_token(TT::Terminator) {
                        rval = RI::RValue(RValue{kind: var_kind, expr: Rc::new(NullExpression{})});
                        // rval = RValue{kind: VarKind::Null, expr: Rc::new(NullExpression{})};
                        break 'match_kind;
                    }                    
                    self.consume(TT::Assign, "expecting '=' after  Type");
                    let _rval = self.expression();
                    rval = RI::RValue(RValue{kind: var_kind, expr: _rval});            
                } else if kind == TT::Assign {  
                    inst_kind = InstructionKind::Assign;    
                    lval = Some(LValue { expr: ident});
                    // @todo check if type has been defined beforehand
                    let expr = self.expression();
                    rval = RI::RValue(RValue{kind: VarKind::Null, expr: expr});  
                } else if kind == TT::Terminator {
                    // in this case there is just and rvalue
                    inst_kind = InstructionKind::Expression;    
                    rval = RI::RValue(RValue{kind: VarKind::Null, expr: ident});  
                } else {                    
                    panic!("invalid token here: {:?}", kind);
                }
            } 
            TT::Continue => {
                inst_kind = InstructionKind::Continue;
                rval = RI::None;
            }
            TT::Break => {
                inst_kind = InstructionKind::Break;
                rval = RI::None;
            }
            TT::Defer => {
                inst_kind = InstructionKind::Defer;
                let defer_scope = self.scope(scope.clone(), proc_rets);
                let mut s = scope.as_ref().borrow_mut();
                s.defers.push(defer_scope.clone());
                // println!("scope len {}, {}", s.defers.len(), s.emit(self, &s));
                drop(s);
                // println!("{}", defer_scope.borrow().emit(&self, &scope.borrow()));
                
                
                // rval = RInstruction::Scope(scope)
                rval = RI::None;
            }
            TT::Return => {
                // return expression;
                inst_kind = InstructionKind::Return;
                let _rval = 
                match self.peek().kind {
                    // return for void functions
                    TT::Terminator | TT::RightBrace => Rc::new(NullExpression{}),
                    // 
                    _ => self.expression()
                };
                let rets = proc_rets.unwrap();
                
                // @todo evaluate for type; check with procedure return
                rval = RI::RValue(RValue{kind: rets.args[0].kind.clone(), expr: _rval});
            } 
            TT::Print => {
                // put back and just parse as an expression
                self.undo();            
                inst_kind = InstructionKind::Expression;            
                let expr = self.expression();
                rval = RI::RValue(RValue{kind: VarKind::Void, expr: expr}); 
            }
            TT::If => {
                inst_kind = InstructionKind::If;
                let expr = self.expression();
                rval = RI::RValue(RValue{kind: VarKind::Bool, expr: expr}); 
            } 
            TT::While => {
                inst_kind = InstructionKind::While;
                let expr = self.expression();
                rval = RI::RValue(RValue{kind: VarKind::Bool, expr: expr}); 
            } 
            TT::For => {
                inst_kind = InstructionKind::For;
                // for it, index: array                
                let a = self.consume(TT::Identifier, "_").literal.cast_string();
                if self.peek_token(TT::Comma) {
                    let b = self.consume(TT::Identifier, "_").literal.cast_string();
                    self.consume(TT::Define, "_");
                    let c = self.consume(TT::Identifier, "_").literal.cast_string();                
                    let instr = self.scope(scope, proc_rets);
                    rval = RInstruction::ForLoop(ForLoop { inner: ForKind::Array([a,b,c]), scope: instr }); 
                } else {
                    // for i: expr..expr
                    self.consume(TT::Define, "_");
                    let from = self.expression();
                    self.consume(TT::Spread, "_");
                    let to = self.expression();
                    let instr = self.scope(scope, proc_rets);
                    rval = RInstruction::ForLoop(ForLoop { inner: ForKind::Range(a, from, to), scope: instr }); 
                }
            } 
            TT::Else => {
                inst_kind = InstructionKind::Else;
                match self.peek().kind {
                    TT::Terminator | TT::If | TT::LeftBrace => {}
                    _ => unreachable!("bad token '{:?}' after else", self.peek().kind),
                }
                // let expr = self.expression();
                rval = RI::RValue(RValue{kind: VarKind::Bool, expr: Rc::new(NullExpression{})}); 
            }
            TT::LeftBrace => {
                inst_kind = InstructionKind::Scope;
                self.undo();
                let expr = self.scope(scope, proc_rets);
                rval = RI::Scope(expr); 
            } 
            _ => {
                panic!("todo: {:?}", first.kind);
            }
        } }
        // @todo
        return Some(ExpInstruction {kind: inst_kind, left: lval, right: rval});
    }
    // proc_rets gets passed if this is a procedure scope
    fn scope(&mut self, parent: ScopeRef, proc_rets: Option<&ExpArgs>) -> ScopeRef {
        let _scope = Rc::new(RefCell::new(Scope::default()));
        let mut scope = (*_scope).borrow_mut();
        scope.parent = Some(parent);
        drop(scope);
        self.consume(TT::LeftBrace, "scope starts with '{'");
        self.consume_terminator();
        while !self.peek_token(TT::RightBrace) {
            self.consume_terminator();
            let inst = self.instruction(_scope.clone(), proc_rets);
            if let Some(inst) = inst {
                let mut scope = (*_scope).borrow_mut();
                scope.instructions.push(inst);
                self.consume_terminator();
            }
        }
        // self.consume(TT::RightBrace, "scope ends with '}'");
        return _scope.clone();
    }
    fn parse_struct(&mut self) {
        let ident = self.consume(TT::Identifier, "need struct name here @todo").clone();
        self.consume(TT::Const, "'::' after struct name");
        self.consume(TT::Struct, "expecting 'struct' after 'struct_name ::'");
        self.consume(TT::LeftBrace, "struct starts with '{'");
        self.consume_terminator();
        let mut vars = Vec::<Var>::new();
        while self.peek().kind == TT::Identifier {            
            let vname = self.consume(TT::Identifier, "ident for var").clone();
            self.consume(TT::Define, ": after struct var");
            let var_kind = self.read_type();
            self.consume(TT::Comma, "expecting ',' inbetween vars in struct");
            vars.push(Var::new(vname.literal.cast_string(), var_kind));
            self.consume_terminator();
        }
        self.consume_terminator();
        self.consume(TT::RightBrace, "struct ends with '}'");
        let name = ident.literal.cast_string();
        self.structs.insert(name.clone(), StructDef { name: name.clone(), vars});
        self.structs_order.push(name.clone());
    }
    fn procedure(&mut self, parent: ScopeRef) {
        let ident = self.consume(TT::Identifier, "need procedure here todo");
        let ident = ident.clone();
        self.consume(TT::Const, ":: after proc def");
        let args = self.procedure_args();
        let mut rets: ExpArgs;
        if self.peek().kind == TT::Arrow {
            self.consume(TT::Arrow, "-> after proc args if there is a return type");
            rets = self.procedure_rets();
        } else {
            rets = ExpArgs{args: vec![Var::new("".into(), VarKind::Void)]};
        }
        
        let _scope = self.scope(parent, Some(&rets));        
        let mut scope = (*_scope).borrow_mut();
        for v in &args.args {
            scope.variables.insert(v.name.clone(), v.clone());
        }
        drop(scope);
        // @todo >.<
        // let scope = Rc::clone(*scope);
        // let scope = *&*scope;
        let proc = Procedure{args, rets, name: ident.literal.cast_str().to_string(), scope: _scope.clone()};
        self.procs.insert(proc.name.clone(), proc);
        
    }
    pub fn expression(&mut self) -> Rc<dyn Expression> {
        let mut left = self.equality();
        
        while self.peek_tokens(&[TT::And, TT::Or]) {
            let op = self.previous().clone();
            let right = self.equality();
            left = Rc::new(ExpBinary::new(op, left, right));
        }
        return left;
    }
    fn equality(&mut self) -> Rc<dyn Expression> {
        let mut left = self.comparison();
        while self.peek_tokens(&[TT::Equal, TT::NotEqual]) {
            let op = self.previous().clone();
            let right = self.comparison();
            left = Rc::new(ExpBinary::new(op, left, right));
        }
        return left;
    }

    fn comparison(&mut self) -> Rc<dyn Expression> {
        let mut left = self.term();
        while self.peek_tokens(&[TT::Greater, TT::GreaterEqual, TT::Less, TT::LessEqual, TT::Mod]) {
            let op = self.previous().clone();
            let right = self.term();
            left = Rc::new(ExpBinary::new(op, left, right));
        }        
        return left;
    }
    fn term(&mut self) -> Rc<dyn Expression> {
        let mut left = self.factor();
        while self.peek_tokens(&[TT::Minus, TT::Plus]) {
            let op = self.previous().clone();
            let right = self.factor();
            left = Rc::new(ExpBinary::new(op, left, right));
        }        
        return left;
    }
    fn factor(&mut self) -> Rc<dyn Expression> {
        let mut left = self.unary();
        while self.peek_tokens(&[TT::Star, TT::Slash]) {
            let op = self.previous().clone();
            let right = self.unary();
            left = Rc::new(ExpBinary::new(op, left, right));
        }        
        return left;
    }    
    fn unary(&mut self) -> Rc<dyn Expression> {
        
        if self.peek_tokens(&[TT::REF, TT::DEREF, TT::Not, TT::Minus]) {
            let op = self.previous().clone();
            let right = self.unary();
            return Rc::new(ExpUnary::new(op, right));
        }        
        return self.subscribe();
    }        
    // a[][][] must be represented right to left
    fn inner_subscribe(&mut self, last: Rc<dyn Expression>) -> Rc<dyn Expression> {
        if self.peek_token(TT::LeftBracket) {
            let expr = self.expression();
            self.consume(TT::RightBracket, "subscription ends with ']'");
            let rc = Rc::new(ExpSubscribe { left: last, inner: expr});
            return self.inner_subscribe(rc);
        }
        return last;
    }
    fn subscribe(&mut self) -> Rc<dyn Expression> {
        let left = self.primary();
        // let mut exp = ExpSubscribe;
        if self.peek_token(TT::LeftBracket) {
            let expr = self.expression();
            self.consume(TT::RightBracket, "subscription ends with ']'");
            let rc = Rc::new(ExpSubscribe{left, inner: expr});            
            return self.inner_subscribe(rc);
        } else {
            return left;
        }        
    }
    fn primary(&mut self) -> Rc<dyn Expression> {
        if self.peek_token(TokenType::String) {
            let exp = ExpLiteral{literal: self.previous().literal.clone()};
            return Rc::new(exp)
        }
        if self.peek_token(TokenType::True) {
            let exp = ExpLiteral{literal: Literal::Bool { val: true }};
            return Rc::new(exp)
        }
        if self.peek_token(TokenType::False) {
            let exp = ExpLiteral{literal: Literal::Bool { val: false }};
            return Rc::new(exp)
        }
        // @todo distinguish Number and Ident properly for type checking
        if self.peek_token(TokenType::Number) {
            let exp = ExpLiteral{literal: self.previous().literal.clone()};
            return Rc::new(exp)
        }
        if self.peek_tokens(&[TT::Identifier, TT::Print]) {
            // proc call
            if !self.finished() && self.peek().kind == TT::LeftParen {
                self.undo();
                let p_call = self.procedure_call();
                return Rc::new(p_call);
            }
            let l = self.previous().literal.cast_string();
            
            // @todo what if 'print * 2'?; no '()'
            let exp = ExpLiteral{literal: self.previous().literal.clone()};
            return Rc::new(exp)            
        }
        
        if self.peek_token(TT::LeftParen) {
            let expr = self.expression();
            self.consume(TT::RightParen, "left ( must have a matching right )");
            return Rc::new(ExpGroup{expression: expr})
        }
        panic!("bad token here {:?}", self.peek().kind);
    }


    fn peek_token(&mut self, token: TokenType) -> bool {
        if self._check_peek(token) {
            self.advance();
            return true;
        }
        return false;
    }    
    fn peek_tokens(&mut self, tokens: &[TokenType]) -> bool {
        for &t in tokens {
            if self._check_peek(t) {
                self.advance();
                return true;
            }
        }
        return false;
    }
    fn _check_peek(&self, token: TokenType) -> bool {
        if self.finished() {
            return false;
        }
        return self.peek().kind == token;
    }

    #[inline]
    fn finished(&self) -> bool {
        return self.current == self.tokens.len();
    }
    fn peek(&self) -> &Token {
        return &self.tokens[self.current];
    }

    fn is_next_struct(&self) -> bool {
        // 0    1    2
        // name :: struct {...}
        let idx = self.current + 2;
        if idx >= self.tokens.len() {return false;}
        return self.tokens[idx].kind == TT::Struct;
    }
    fn previous(&self) -> &Token {
        return &self.tokens[self.current-1];
    }
    
    fn read_type(&mut self) -> VarKind {
        let t = self.consume_next();
        match t.kind {
            TT::POINTER => {
                VarKind::Pointer(Box::new(self.read_type()))
            }
            TT::LeftBracket => {
                self.consume(TT::Spread, "@todo other array options");
                self.consume(TT::RightBracket, "end array with ']'");
                VarKind::Array(Box::new(self.read_type()))
            }
            TT::Identifier => {
                let s = t.literal.cast_string();
                VarKind::_basic_from_string(&s)
            }
            TT::Void => VarKind::Void,
            _ => panic!("bad token for type: {:?}", t.kind),
        }
        
    }
    // doesn't include the token
    fn subscription_read_string_until_token(&mut self, kind: TokenType) -> String {
        let mut s = String::new();
        loop {
            let t = self.consume_next();
            match t.kind {
                kind => return s,
                TT::LeftBracket => s += &"[",
                TT::RightBracket => s += &"]",
                _ => unreachable!("???"),
            }
        }
    }
    // includes ident
    fn read_string_until_ident(&mut self) -> String {
        let mut s = String::new();
        loop {
            let t = self.consume_next();
            match t.kind {
                TT::DEREF => s += emit::DEREF,
                TT::Identifier => {
                    s += &t.literal.cast_string();
                    return s;
                }
                _ => unreachable!("invalid lvalue expression"),
            }
        }
    }
    fn consume_next(&mut self) -> &Token {
        self.advance();
        return self.previous();
    }
    // returns token that comes *after* kind
    /* fn consume_next(&mut self, kind: TokenType, msg: &str) -> &Token {
        if self.check_single(kind) {
            return self.advance();
        }
        panic!("{} \n read: {:?}\n", msg, self.peek().kind);
    } */
    fn consume(&mut self, kind: TokenType, msg: &str) -> &Token {
        if self._check_peek(kind) {
            return self.advance();
        }
        panic!("{} \n ~~~ read: {:?} ~~~ \n", msg, self.peek().kind);
    }
    // undos an advance
    fn undo(&mut self) {
        debug_assert!(self.current > 0);
        self.current -=1;
    }
    fn advance(&mut self) -> &Token {
        if !self.finished() {
            self.current += 1;
        }
        return self.previous();
    }
}