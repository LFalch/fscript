use lrlex::lrlex_mod;
use lrpar::lrpar_mod;

use super::ast::Statements;

lrlex_mod!("source/fal.l");
lrpar_mod!("source/fal.y");

pub fn parse(s: &str) -> Option<Statements> {
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

    for e in errs {
        eprintln!("{}", e.pp(&lexer, &fal_y::token_epp));
    }

    res
}
