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
  | 'FN' Identifier 'LPAREN' Vars 'RPAREN' Expr { Statement::Function(FileSpan::new($lexer, $span), $2, Binding::tuple($4.0), Type::tuple($4.1), $6) }
  | Expr 'EQ' Expr { Statement::Reassign(FileSpan::new($lexer, $span), ReassignLhs::from_expr($1).unwrap(), $3) }
  | 'RET' Expr { Statement::Return(FileSpan::new($lexer, $span), $2) }
  | Expr { Statement::DiscardExpr($1) }
  ;

Vars -> (Vec<Binding>, Vec<Type>):
    Var 'COMMA' Vars { {let (mut bs, mut ts) = $3; bs.insert(0, $1.0); ts.insert(0, $1.1); (bs, ts)} }
  | Var { (vec![$1.0], vec![$1.1]) }
  | { (Vec::new(), Vec::new()) }
  ;

Var -> (Binding, Type):
    'ID' { (Binding::Name(get_str($lexer, $1)), Type::Inferred) }
  | 'ID' 'COLON' Type { (Binding::Name(get_str($lexer, $1)), Type::Named($3)) }
  | 'LPAREN' Vars 'RPAREN' { (Binding::tuple($2.0), Type::tuple($2.1)) }
  ;

Type -> NamedType:
    'BOOL' { NamedType::Bool }
  | 'UINT' { NamedType::Uint }
  | 'INT' { NamedType::Int }
  | 'FLOAT' { NamedType::Float }
  | 'STRING' { NamedType::String }
  | 'QUEST' Type { NamedType::Option(Box::new($2)) }
  | 'AMP' Type { NamedType::Reference(Box::new($2)) }
  | 'LBRACK' Type 'RBRACK' { NamedType::Array(Box::new($2)) }
  | 'FN' TupleType 'RET' Type { NamedType::Function(Box::new($2), Box::new($4)) }
  ;

TupleType -> NamedType:
   'LPAREN' Types 'RPAREN' {
        match $2.len() {
            0 => NamedType::Unit,
            1 => $2.pop().unwrap(),
            _ => NamedType::Tuple($2)
        }
    }
  ;

Types -> Vec<NamedType>:
    Type 'COMMA' Types { {let mut v = $3; v.insert(0, $1); v} }
  | Type { vec![$1] }
  | { Vec::new() }
  ;

Exprs -> Vec<Expr>:
    Expr 'COMMA' Exprs { {let mut v = $3; v.insert(0, $1); v} }
  | Expr { vec![$1] }
  | { Vec::new() }
  ;

Identifier -> String:
    'ID' { get_str($lexer, $1) }
  | 'BOOL' { "bool".to_owned() }
  | 'UINT' { "uint".to_owned() }
  | 'INT' { "int".to_owned() }
  | 'FLOAT' { "float".to_owned() }
  | 'STRING' { "string".to_owned() }
  ;

Expr -> Expr:
    'IF' Expr 'COLON' Expr 'ELSE' Expr 'DOT' { Expr::If(FileSpan::new($lexer, $span), Box::new($2), Box::new($4), Box::new($6)) }
  | 'WHILE' Expr 'COLON' Expr 'DOT' { Expr::While(FileSpan::new($lexer, $span), Box::new($2), Box::new($4)) }
  | Identifier Tuple { Expr::Call(FileSpan::new($lexer, $span), $1, Box::new($2)) }
  | Expr 'DOT' 'ID' 'LPAREN' Exprs 'RPAREN' { {
        let mut v = $5;
        v.insert(0, $1);

        let args_fs = FileSpan::from_lexeme($lexer, $4).ended_by(FileSpan::from_lexeme($lexer, $6));

        Expr::Call(FileSpan::new($lexer, $span), get_str($lexer, $3), Box::new(Expr::Tuple(args_fs, v)))
    } }
  | Expr 'DOT' 'ID' { Expr::Member(FileSpan::new($lexer, $span), Box::new($1), get_str($lexer, $3)) }
  | Expr 'DOT' 'LBRACK' Expr 'RBRACK' { Expr::Index(FileSpan::new($lexer, $span), Box::new($1), Box::new($4)) }
  | 'SOME' 'LPAREN' Expr 'RPAREN' { Expr::Some(FileSpan::new($lexer, $span), Box::new($3)) }
  | Expr 'PLUS' Expr { binop($lexer, $span, "add", $1, $3) }
  | Expr 'MINUS' Expr { binop($lexer, $span, "sub", $1, $3) }
  | Expr 'MUL' Expr { binop($lexer, $span, "mul", $1, $3) }
  | Expr 'DIV' Expr { binop($lexer, $span, "div", $1, $3) }
  | Expr 'MOD' Expr { binop($lexer, $span, "rem", $1, $3) }
  | Expr 'CONCAT' Expr { binop($lexer, $span, "concat", $1, $3) }
  | Expr 'POW' Expr { binop($lexer, $span, "pow", $1, $3) }
  | Expr 'EQT' Expr { binop($lexer, $span, "eq", $1, $3) }
  | Expr 'NEQ' Expr { binop($lexer, $span, "neq", $1, $3) }
  | Expr 'GT' Expr { binop($lexer, $span, "gt", $1, $3) }
  | Expr 'GTE' Expr { binop($lexer, $span, "gte", $1, $3) }
  | Expr 'LT' Expr { binop($lexer, $span, "lt", $1, $3) }
  | Expr 'LTE' Expr { binop($lexer, $span, "lte", $1, $3) }
  | Expr 'SHL' Expr { binop($lexer, $span, "shl", $1, $3) }
  | Expr 'SHR' Expr { binop($lexer, $span, "shr", $1, $3) }
  | Expr 'AMP' Expr { binop($lexer, $span, "bitand", $1, $3) }
  | Expr 'HAT' Expr { binop($lexer, $span, "xor", $1, $3) }
  | Expr 'PIPE' Expr { binop($lexer, $span, "bitor", $1, $3) }
  | Expr 'OR' Expr { binop($lexer, $span, "or", $1, $3) }
  | Expr 'AND' Expr { binop($lexer, $span, "and", $1, $3) }
  | Identifier { Expr::Identifer(FileSpan::new($lexer, $span), $1) }
  | Primitive { Expr::Constant(FileSpan::new($lexer, $span), $1) }
  | 'LBRACK' Exprs 'RBRACK' { Expr::Array(FileSpan::new($lexer, $span), $2) }
  | Tuple { $1 }
  | 'LBRACE' Statements 'RBRACE' { Expr::Block(FileSpan::new($lexer, $span), $2) }
  | 'MINUS' Expr { Expr::Call(FileSpan::new($lexer, $span), "neg".to_owned(), Box::new($2)) }
  | 'EXCL' Expr { Expr::Call(FileSpan::new($lexer, $span), "not".to_owned(), Box::new($2)) }
  | 'MUL' Expr { Expr::Deref(FileSpan::new($lexer, $span), Box::new($2)) }
  | 'AMP' Expr { Expr::Ref(FileSpan::new($lexer, $span), Box::new($2)) }
  | 'AT' Expr { Expr::MutRef(FileSpan::new($lexer, $span), Box::new($2)) }
  ;

Tuple -> Expr:
  'LPAREN' Exprs 'RPAREN' {
        match $2.len() {
            0 => Expr::Constant(FileSpan::new($lexer, $span), Primitive::Unit),
            1 => $2.pop().unwrap(),
            _ => Expr::Tuple(FileSpan::new($lexer, $span), $2)
        }
    }
  ;

Primitive -> Primitive:
    'INT_LITERAL' { parse_int(gets($lexer, $1)) }
  | 'FLOATING_LITERAL' { Primitive::Float(gets($lexer, $1).parse().unwrap()) }
  | 'TRUE_LITERAL' { Primitive::Bool(true) }
  | 'FALSE_LITERAL' { Primitive::Bool(false) }
  | 'NONE_LITERAL' { Primitive::None }
  | 'STRING_LITERAL' { Primitive::String(parse_string_literal(gets($lexer, $1))) }
  ;

%%
use crate::source::ast::*;
use crate::source::FileSpan;
use crate::types::Type as NamedType;

use lrpar::{NonStreamingLexer, Span};
use lrlex::DefaultLexeme;

pub type Statements = Vec<Statement>;

fn binop(lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>, span: Span, binop: &str, a: Expr, b: Expr) -> Expr {
    let fs = FileSpan::new(lexer, span);
    Expr::Call(fs, binop.to_owned(), Box::new(Expr::Tuple(fs, vec![a, b])))
}

fn parse_int(s: &str) -> Primitive {
    match (s.parse::<u64>(), s.parse::<i64>()) {
        (Ok(u), Ok(_)) => Primitive::AmbigInt(u),
        (Ok(u), Err(_)) => Primitive::Uint(u),
        (Err(_), Ok(i)) => Primitive::Int(i),
        (Err(_), Err(_)) => unreachable!("could not convert integer literal"),
    }
}

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
