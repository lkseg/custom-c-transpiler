use std::{fs, iter::Peekable, str::Chars, collections::{HashMap, HashSet}};
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TokenType {
    Error, LeftBrace, RightBrace , LeftParen, RightParen, LeftBracket, RightBracket, 
    
    Terminator, Identifier,
    String,
    Minus, Plus,
    MinusEq, PlusEq,
    Mod,
    And, Or, BitAnd, BitOr,
    Spread, // ..
    Define, // ':'
    Const, // '::'
    Arrow,
    Assign,
    DefAssign,
    Number,
    Star, // Asterisk
    Slash,
    Equal, NotEqual,
    Greater, GreaterEqual, Less, LessEqual,
    Continue,
    Break,
    If,
    Else,
    For,
    While,
    Not,
    Void,
    Struct,
    Return,
    Print,
    Comma, Dot,
    True, False,
    Apostrophe,
    Caret,
    Defer,
    LeftAngle, RightAngle, // @todo
}
impl TokenType {
    pub const POINTER: TokenType = TokenType::Star; // @todo cleanup stars
    pub const REF: TokenType = TokenType::BitAnd;
    pub const DEREF: TokenType = TokenType::Star;
}
type TT = TokenType;




pub struct Scanner {
    pub keywords: HashMap<String, TokenType>,
    pub tokens: Vec<Token>,
}
impl Scanner {
    pub fn new() -> Self {
        // @todo
        // l = get_keyword(word); if l.is_some(); add_token(l); else add_ident(word)
        let mut k = HashMap::new();
        k.insert("void".into(), TokenType::Void);
        k.insert("return".into(), TokenType::Return);        
        k.insert("if".into(), TokenType::If);        
        k.insert("else".into(), TokenType::Else);
        k.insert("for".into(), TokenType::For);
        k.insert("while".into(), TokenType::While);
        k.insert("print".into(), TokenType::Print);
        k.insert("struct".into(), TokenType::Struct);
        k.insert("true".into(), TokenType::True);
        k.insert("false".into(), TokenType::False);
        k.insert("continue".into(), TokenType::Continue);
        k.insert("break".into(), TokenType::Break);
        k.insert("defer".into(), TokenType::Defer);
        Self {
            keywords: k,
            tokens: Default::default(),
        }
    }
}
impl Default for TokenType {fn default() -> Self {TokenType::Error}}
#[derive(Clone, Debug)]
pub enum Literal {
    None,
    Identifier{name: String},
    String{string: String},
    SInt{num: i64},
    UInt{num: u64},
    Float{num: f64},
    Bool{val: bool},
}
impl Default for Literal {fn default() -> Self {Self::None{}}}
impl Literal {
    pub fn emit(&self) -> String {
        match self {
            Self::Float { num } => num.to_string(),
            Self::SInt { num } => num.to_string(),
            Self::Bool { val } => if *val {"true".into()} else {"false".into()},
            Self::Identifier { name } => name.clone(),
            Self::String { string } => format!("\"{}\"", string),
            _ => unreachable!("todo"),
        }
    }
    pub fn cast_str(&self) -> &str {
        if let Literal::Identifier { name } = self {
            // @todo rval type
            return name;
        } else {
            panic!("can't cast {:?}", self);
        }
    }
    pub fn cast_string(&self) -> String {
        return self.cast_str().to_string();
    }
}
#[derive(Default, Clone, Debug)]
pub struct Token {
    pub kind: TokenType,
    pub literal: Literal,
    // lexeme: String,
}
impl Token {
    pub fn emit(&self) -> String {
        match self.kind {
            TokenType::DefAssign => "=".into(),
            TokenType::Assign => "=".into(),
            TokenType::Plus => "+".into(),
            TokenType::Mod => "%".into(),
            TokenType::Minus => "-".into(),            
            TokenType::Star => "*".into(),
            TokenType::Slash => "/".into(),
            _ => {
                unreachable!("todo");
            }
        }
    }
    pub fn new(kind: TokenType, literal: Literal) -> Self {
        Self {
            kind,
            literal,
        }
    }
    pub fn single(kind: TokenType) -> Self {
        return Self {
            kind,
            ..Default::default()
        }
    }
    pub fn ident(name: String) -> Self {
        return Self {
            kind: TokenType::Identifier,
            literal: Literal::Identifier {name},
        }
    }    
}
const COMMA : char = ',';
const LEFT_PAREN : char = '(';
const RIGHT_PAREN: char = ')';

const LEFT_BRACKET : char = '[';
const RIGHT_BRACKET: char = ']';

const LEFT_ANGLE : char = '<';
const RIGHT_ANGLE: char = '>';

const LEFT_BRACE : char = '{';
const RIGHT_BRACE: char = '}';

const TERMINATOR : char = '\n';
const SEPERATOR  : char = ' ';

const DQM : char = '"';
 
const AND: char = '&';
const OR : char = '|';

const NOT        : char = '!';
const DOT        : char = '.';
const DEFINE     : char = ':';
const CONST      : &str = "::";

const MINUS     : char = '-';
const RETURN    : &str = "->";

const ASSIGN     : char = '=';
const STAR       : char = '*';
const PLUS       : char = '+';
const MOD       : char = '%';
const SLASH      : char = '/';

const EQUAL    : &str = "==";
const NOTEQUAL : &str = "!=";

const GREATER: char = '>';
const LESS: char = '<';

const APOSTROPHE: char = '\'';

const CARET: char = '^';


type ScanIter<'a> = Peekable<Chars<'a>>;


#[inline]
pub fn is_stop(c: char) -> bool {
    // @note DOT is not listed
    match c {
        DQM | MOD | NOT | GREATER | LESS | AND | OR | SEPERATOR | TERMINATOR | DEFINE | COMMA |
        STAR | PLUS | SLASH | MINUS |
        LEFT_BRACE | LEFT_PAREN | RIGHT_BRACE | RIGHT_PAREN | LEFT_BRACKET | RIGHT_BRACKET
        => true,
        _ => false,
    }
}

pub fn scan_number(iter: &mut ScanIter) -> Token {
    let mut word = String::new();
    let mut is_float = false;
    let mut is_first_dot = true;
    
    while let Some(&c) = iter.peek() {
        if c == DOT {
            let mut s = iter.clone();
            // current: DOT
            s.next();
            if s.next() == Some('.') {
                break;
            }
            is_float = true;
        }
        if is_stop(c) || (!c.is_numeric() && c != DOT) {
            break;
        }
        iter.next();
        word.push(c);
    }
    if is_float {
        let num = word.parse::<f64>().unwrap();
        return Token::new(TokenType::Number, Literal::Float{num: num as f64});
    }
    let num = word.parse::<i64>().unwrap();
    return Token::new(TokenType::Number, Literal::SInt{num: num as i64});
}
pub fn scan_word(iter: &mut ScanIter) -> Token {
    let mut word = String::new();
    while let Some(&c) = iter.peek() {
        if is_stop(c) {
            break;
        }
        iter.next();
        word.push(c);
    }
    return Token::new(TokenType::Identifier, Literal::Identifier { name: word});
}
pub fn scan_string(iter: &mut ScanIter) -> Token {
    let mut word = String::new();
    assert!(iter.next().unwrap() == '"');
    while let Some(&c) = iter.peek() {
        if c == '"' {
            break;
        }
        iter.next();
        word.push(c);
    }
    assert!(iter.next().unwrap() == '"');
    return Token::new(TokenType::String, Literal::String { string: word});
}
impl Scanner {

    pub fn scan(&mut self, s: String) {
    
        let mut word = String::new(); 
        let mut tokens = Vec::new();
        let mut iter = s.chars().peekable();
        let mut last_type = TokenType::Error;
        loop {
            let c = iter.peek();
            if c.is_none() {
                break;            
            }
            let c = *c.unwrap();
            // println!("{c}");
            let is_alpha = c.is_alphabetic();
    
            if is_alpha || c == '_' {
                let mut token = scan_word(&mut iter);
                let Literal::Identifier { name } = &token.literal else {panic!()};
                if let Some(&kind) = self.keywords.get(name) {
                    match kind {
                        TT::Defer | TT::Continue | TT::Break | TT::True | TT::False | TT::For | TT::While | TT::Struct | TT::Void | TT::Return | TT::If | TT::Else | TT::Print => {
                            token = Token::single(kind);                            
                        }
                        _ => panic!("")
                    }
                }
                tokens.push(token);
                continue;
            } else if c.is_numeric() {
                let token = scan_number(&mut iter);
                tokens.push(token);
                continue;
            } else if c == '"' {
                let token = scan_string(&mut iter);
                tokens.push(token);
                continue;
            }
            // peeked value == current
            iter.next();
            let mut is_sep = false;
            let token = match c {
                COMMA => {
                    Token::single(TokenType::Comma)
                }
                LEFT_BRACKET => Token::single(TokenType::LeftBracket),
                RIGHT_BRACKET => Token::single(TokenType::RightBracket),
                // LEFT_ANGLE => Token::single(TokenType::LeftAngle),
                // RIGHT_ANGLE => Token::single(TokenType::RightAngle),
                LEFT_PAREN => {
                    Token::single(TokenType::LeftParen)
                }
                RIGHT_PAREN => {
                    Token::single(TokenType::RightParen)
                }
                LEFT_BRACE => {
                    Token::single(TokenType::LeftBrace)
                }
                RIGHT_BRACE => {
                    Token::single(TokenType::RightBrace)
                }
                DOT => {
                    let p = *iter.peek().unwrap();
                    if p == DOT {
                        iter.next();
                        Token::single(TokenType::Spread)
                    } else {
                        Token::single(TokenType::Dot)
                    }
                }

                TERMINATOR => {
                    // terminators are idempotent
                    if last_type == TokenType::Terminator {
                        continue;
                    }
                    Token::single(TokenType::Terminator)
                } 
                AND => {
                    let p = *iter.peek().unwrap();
                    if p == AND {
                        iter.next();
                        Token::single(TokenType::And)
                    } else {
                        Token::single(TokenType::BitAnd)
                    }
                }
                OR => {
                    let p = *iter.peek().unwrap();
                    if p == OR {
                        iter.next();
                        Token::single(TokenType::Or)
                    } else {
                        Token::single(TokenType::BitOr)
                    }
                }
                DEFINE => {                
                    let p = *iter.peek().unwrap();
                    
                    if p == DEFINE {
                        iter.next();
                        // ::
                        Token::single(TokenType::Const)
                    } else if p == ASSIGN {
                        iter.next();
                        // :=
                        Token::single(TokenType::DefAssign)
                    }
                    else {
                        // : followed by ' Type ='
                        Token::single(TokenType::Define)
                    }
                }
                ASSIGN => {
                    let p = *iter.peek().unwrap();
                    
                    if p == ASSIGN {
                        // ==
                        iter.next();
                        Token::single(TokenType::Equal)
                    } else {
                        Token::single(TokenType::Assign)
                    }
                }
                GREATER => {
                    let p = *iter.peek().unwrap();
                    // >=
                    if p == ASSIGN {
                        iter.next();
                        Token::single(TT::GreaterEqual)
                    } else {
                        Token::single(TT::Greater)
                    }
                }
                NOT => {
                    let p = *iter.peek().unwrap();
                    // >=
                    if p == ASSIGN {
                        iter.next();
                        Token::single(TT::NotEqual)
                    } else {
                        Token::single(TT::Not)
                    }                    
                }
                LESS => {
                    let p = *iter.peek().unwrap();
                    // >=
                    if p == ASSIGN {
                        iter.next();
                        Token::single(TT::LessEqual)
                    } else {
                        Token::single(TT::Less)
                    }
                }
                STAR => {
                    Token::single(TokenType::Star)
                }
                MOD => Token::single(TokenType::Mod),
                PLUS => {
                    let p = *iter.peek().unwrap();
                    if p == ASSIGN {
                        iter.next();                        
                        Token::single(TokenType::PlusEq)
                    } else {
                        Token::single(TokenType::Plus)
                    }
                    
                }
                SLASH => {
                    let p = *iter.peek().unwrap();
                    if p == SLASH {
                        while *iter.peek().unwrap() != TERMINATOR {
                            iter.next();
                        }
                        // iter.next();
                        continue;
                    } else {
                        Token::single(TokenType::Slash)
                    }
                }
                MINUS => {
                    if *iter.peek().unwrap() == '>' {
                        iter.next();
                        Token::single(TokenType::Arrow)
                    } else if *iter.peek().unwrap() == '=' {
                        iter.next();
                        Token::single(TokenType::MinusEq)
                    }
                    else {
                        Token::single(TokenType::Minus)
                    }
                }    
                SEPERATOR => {
                    // Token::default();
                    continue;
                } 
                APOSTROPHE => Token::single(TokenType::Apostrophe),
                CARET => Token::single(TokenType::Caret),
                _ => {
                    panic!("unexpected character '{}'", c);
                }            
            };

            last_type = token.kind;
            tokens.push(token);      
    
        }   
        self.tokens = tokens;
    }    
}