pub mod ast;
pub mod check;

use crate::source::ast::Statements as SourceStatements;
use self::ast::{Type, Statements as TypedStatements};
use self::check::{check_statements, SymbolTable, TypeCollection, ReturnType, TypeError};

pub fn type_check(stmnts: SourceStatements, function_table: impl IntoIterator<Item=(String, (Vec<Type>, Type))>) -> Result<(ReturnType, TypedStatements), TypeError> {
    let mut st = SymbolTable::new();

    for (n, (args_type, ret_type)) in function_table {
        st.insert(n, (false, Type::Function(args_type, Box::new(ret_type))));
    }

    check_statements(stmnts, &mut st, &mut TypeCollection::new())
}
