use std::fmt::{self, Display};

use lrlex::lrlex_mod;
use lrpar::lrpar_mod;

use super::ast::Statements;

lrlex_mod!("source/fal.l");
lrpar_mod!("source/fal.y");

pub struct ParseError {
    inner: Vec<String>,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for e in &self.inner {
            writeln!(f, "{e}")?;
        }
        Ok(())
    }
}

pub fn parse(s: &str) -> Result<Statements, ParseError> {
    let lexerdef = fal_l::lexerdef();

    let lexer = lexerdef.lexer(s);
    
    // for lex in lexer.iter() {
    //     use lrlex::LexerDef;
    //     use lrpar::Lexeme;
    //     use lrpar::Lexer;
    //     match lex {
    //         Ok(lex) => {
    //             let span = lex.span();
    //             if lex.faulty() {
    //                 print!("!! ");
    //             }
    //             print!("{} ", lexerdef.get_rule_by_id(lex.tok_id()).name.as_ref().unwrap());
    //             println!("{:?}", &s[span.start()..span.end()]);
    //         }
    //         Err(e) => eprintln!("!!!!!! ERR: {:?}", e),
    //     }
    // }
    
    let (res, errs) = fal_y::parse(&lexer);

    let pe = ParseError {
        inner: errs
            .into_iter()
            .map(|e| {
                e.pp(&lexer, &fal_y::token_epp)
            })
            .collect()
    };

    if !pe.inner.is_empty() {
        return Err(pe);
    }

    res.ok_or(pe)
}
