%start Start

%token 'UNMATCHED'

%left 'COLON'
%left 'COMMA'

%nonassoc 'FN'
%nonassoc 'AMP' 'AT'
%left 'OR'
%left 'AND'
%nonassoc 'EXCL'
%left 'EQT' 'NEQ' 'GT' 'GTE' 'LT' 'LTE'
%left 'PIPE'
%left 'HAT'
%left 'SHL' 'SHR'
%left 'CONCAT'
%left 'PLUS' 'MINUS'
%left 'MUL' 'DIV' 'MOD'
%left 'POW'

%left 'DOT'
%right 'EQ'

%%

Start -> Statements:
    Statements { $1 }
  ;

Statements -> Statements:
    Statement 'SEMICOLON' Statements { {let mut v = $3; v.insert(0, $1); v} }
  | Statement { vec![$1] }
  | { Vec::new() }
  ;

Statement -> Statement:
    'VAR' Var 'EQ' Expr { Statement::VarAssign(FileSpan::new($lexer, $span), $2.0, $2.1, $4) }
  | 'LET' Var 'EQ' Expr { Statement::ConstAssign(FileSpan::new($lexer, $span), $2.0, $2.1, $4) }
  | 'FN' 'ID' 'LPAREN' Vars 'RPAREN' Expr { Statement::Function(FileSpan::new($lexer, $span), get_str($lexer, $2), $4, $6) }
  | 'ID' 'EQ' Expr { Statement::Reassign(FileSpan::new($lexer, $span), get_str($lexer, $1).to_owned(), $3) }
  | 'RET' Expr { Statement::Return(FileSpan::new($lexer, $span), $2) }
  | Expr { Statement::DiscardExpr($1) }
  ;

Vars -> Vars:
    Var 'COMMA' Vars { {let mut v = $3; v.insert(0, $1); v} }
  | Var { vec![$1] }
  | { Vec::new() }
  ;

Var -> Var:
    'ID' { (get_str($lexer, $1), Type::Inferred) }
  | 'ID' 'COLON' Type { (get_str($lexer, $1), Type::Concrete($3)) }
  ;

Type -> ConcreteType:
    'BOOL' { ConcreteType::Bool }
  | 'UINT' { ConcreteType::Uint }
  | 'INT' { ConcreteType::Int }
  | 'FLOAT' { ConcreteType::Float }
  | 'QUEST' Type { ConcreteType::Option(Box::new($2)) }
  | 'AMP' Type { ConcreteType::Reference(Box::new($2)) }
  | 'LBRACK' Type 'SEMICOLON' 'INT_LITERAL' 'RBRACK' { ConcreteType::Array(Box::new($2), gets($lexer, $4).parse().unwrap()) }
  | 'LPAREN' Types 'RPAREN' {
        match $2.len() {
            0 => ConcreteType::Unit,
            1 => $2.pop().unwrap(),
            _ => ConcreteType::Tuple($2)
        }
    }
  | 'FN' 'LPAREN' Types 'RPAREN' 'RET' Type { ConcreteType::Function($3, Box::new($6)) }
  ;

Types -> Vec<ConcreteType>:
    Type 'COMMA' Types { {let mut v = $3; v.insert(0, $1); v} }
  | Type { vec![$1] }
  | { Vec::new() }
  ;

Exprs -> Vec<Expr>:
    Expr 'COMMA' Exprs { {let mut v = $3; v.insert(0, $1); v} }
  | Expr { vec![$1] }
  | { Vec::new() }
  ;

Expr -> Expr:
    'IF' Expr 'COLON' Expr 'ELSE' Expr 'DOT' { Expr::If(FileSpan::new($lexer, $span), Box::new($2), Box::new($4), Box::new($6)) }
  | 'WHILE' Expr 'COLON' Expr 'DOT' { Expr::While(FileSpan::new($lexer, $span), Box::new($2), Box::new($4)) }
  | 'ID' 'LPAREN' Exprs 'RPAREN' { Expr::Call(FileSpan::new($lexer, $span), get_str($lexer, $1), $3) }
  | Expr 'DOT' 'ID' 'LPAREN' Exprs 'RPAREN' { { let mut v = $5; v.insert(0, $1); Expr::Call(FileSpan::new($lexer, $span), get_str($lexer, $3), v) } }
  | Expr 'DOT' 'ID' { Expr::Member(FileSpan::new($lexer, $span), Box::new($1), get_str($lexer, $3)) }
  | Expr 'DOT' 'LBRACK' Expr 'RBRACK' { Expr::Index(FileSpan::new($lexer, $span), Box::new($1), Box::new($4)) }
  | 'SOME' 'LPAREN' Expr 'RPAREN' { Expr::Some(FileSpan::new($lexer, $span), Box::new($3)) }
  | Expr 'PLUS' Expr { Expr::Call(FileSpan::new($lexer, $span), "add".to_owned(), vec![$1, $3]) }
  | Expr 'MINUS' Expr { Expr::Call(FileSpan::new($lexer, $span), "sub".to_owned(), vec![$1, $3]) }
  | Expr 'MUL' Expr { Expr::Call(FileSpan::new($lexer, $span), "mul".to_owned(), vec![$1, $3]) }
  | Expr 'DIV' Expr { Expr::Call(FileSpan::new($lexer, $span), "div".to_owned(), vec![$1, $3]) }
  | Expr 'MOD' Expr { Expr::Call(FileSpan::new($lexer, $span), "rem".to_owned(), vec![$1, $3]) }
  | Expr 'CONCAT' Expr { Expr::Call(FileSpan::new($lexer, $span), "concat".to_owned(), vec![$1, $3]) }
  | Expr 'POW' Expr { Expr::Call(FileSpan::new($lexer, $span), "pow".to_owned(), vec![$1, $3]) }
  | Expr 'EQT' Expr { Expr::Call(FileSpan::new($lexer, $span), "eq".to_owned(), vec![$1, $3]) }
  | Expr 'NEQ' Expr { Expr::Call(FileSpan::new($lexer, $span), "neq".to_owned(), vec![$1, $3]) }
  | Expr 'GT' Expr { Expr::Call(FileSpan::new($lexer, $span), "gt".to_owned(), vec![$1, $3]) }
  | Expr 'GTE' Expr { Expr::Call(FileSpan::new($lexer, $span), "gte".to_owned(), vec![$1, $3]) }
  | Expr 'LT' Expr { Expr::Call(FileSpan::new($lexer, $span), "lt".to_owned(), vec![$1, $3]) }
  | Expr 'LTE' Expr { Expr::Call(FileSpan::new($lexer, $span), "lte".to_owned(), vec![$1, $3]) }
  | Expr 'SHL' Expr { Expr::Call(FileSpan::new($lexer, $span), "shl".to_owned(), vec![$1, $3]) }
  | Expr 'SHR' Expr { Expr::Call(FileSpan::new($lexer, $span), "shr".to_owned(), vec![$1, $3]) }
  | Expr 'AMP' Expr { Expr::Call(FileSpan::new($lexer, $span), "bitand".to_owned(), vec![$1, $3]) }
  | Expr 'HAT' Expr { Expr::Call(FileSpan::new($lexer, $span), "xor".to_owned(), vec![$1, $3]) }
  | Expr 'PIPE' Expr { Expr::Call(FileSpan::new($lexer, $span), "bitor".to_owned(), vec![$1, $3]) }
  | Expr 'OR' Expr { Expr::Call(FileSpan::new($lexer, $span), "or".to_owned(), vec![$1, $3]) }
  | Expr 'AND' Expr { Expr::Call(FileSpan::new($lexer, $span), "and".to_owned(), vec![$1, $3]) }
  | 'ID' { Expr::Identifer(FileSpan::new($lexer, $span), get_str($lexer, $1)) }
  | Primitive { Expr::Constant(FileSpan::new($lexer, $span), $1) }
  | 'LBRACK' Exprs 'RBRACK' { Expr::Array(FileSpan::new($lexer, $span), $2) }
  | 'LPAREN' Exprs 'RPAREN' {
        match $2.len() {
            0 => Expr::Constant(FileSpan::new($lexer, $span), Primitive::Unit),
            1 => $2.pop().unwrap(),
            _ => Expr::Tuple(FileSpan::new($lexer, $span), $2)
        }
    }
  | 'LBRACE' Statements 'RBRACE' { Expr::Block(FileSpan::new($lexer, $span), $2) }
  | 'MINUS' Expr { Expr::Call(FileSpan::new($lexer, $span), "neg".to_owned(), vec![$2]) }
  | 'EXCL' Expr { Expr::Call(FileSpan::new($lexer, $span), "not".to_owned(), vec![$2]) }
  | 'MUL' Expr { Expr::Deref(FileSpan::new($lexer, $span), Box::new($2)) }
  | 'AMP' Expr { Expr::Ref(FileSpan::new($lexer, $span), Box::new($2)) }
  | 'AT' Expr { Expr::MutRef(FileSpan::new($lexer, $span), Box::new($2)) }
  ;

Primitive -> Primitive:
    'INT_LITERAL' { Primitive::Int(gets($lexer, $1).parse().unwrap()) }
  | 'FLOATING_LITERAL' { Primitive::Float(gets($lexer, $1).parse().unwrap()) }
  | 'TRUE_LITERAL' { Primitive::Bool(true) }
  | 'FALSE_LITERAL' { Primitive::Bool(false) }
  | 'NONE_LITERAL' { Primitive::None }
  | 'STRING_LITERAL' { Primitive::String(parse_string_literal(gets($lexer, $1))) }
  ;

%%
use crate::source::ast::*;
use crate::source::FileSpan;
use crate::types::Type as ConcreteType;

use lrpar::NonStreamingLexer;
use lrlex::DefaultLexeme;

pub type Var = (String, Type);
pub type Vars = Vec<Var>;

pub type Statements = Vec<Statement>;

fn parse_string_literal(s: &str) -> String {
    let mut ret = String::with_capacity(s.len());
    let mut escape = false;

    for c in s[1..s.len()-1].chars() {
      if escape {
          ret.push(escape_char(c));
      } else if c == '\\' {
          escape = true;
      } else {
          ret.push(c);
      }
    }

    ret
}

fn escape_char(c: char) -> char {
    match c {
        'n' => '\n',
        '"' => '\"',
        '\'' => '\'',
        '\\' => '\\',
        '0' => '\0',
        'r' => '\r',
        't' => '\t',
        _ => '\u{FFFD}',
    }
}

fn get_str(lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>, lexeme: Result<DefaultLexeme, DefaultLexeme>) -> String {
    gets(lexer, lexeme).to_owned()
}

fn gets<'a>(lexer: &'a dyn NonStreamingLexer<DefaultLexeme, u32>, lexeme: Result<DefaultLexeme, DefaultLexeme>) -> &'a str {
    lexer.span_str(lexeme.unwrap_or_else(|e| e).span())
}
