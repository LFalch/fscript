pub mod ast;
pub mod check;

use crate::source::ast::Statements as SourceStatements;
use crate::source::ast::Type as TypeHint;
use crate::types::Type;
use self::ast::{Statements as TypedStatements};
use self::check::{check_statements, SymbolTable, TypeCollection, ReturnType, TypeError};

pub fn type_check(stmnts: SourceStatements, function_table: impl IntoIterator<Item=(String, (Type, Type))>) -> Result<(ReturnType, TypedStatements), TypeError> {
    let mut st = SymbolTable::new();

    let mut tv = TypeCollection::new();

    for (n, (arg_type, ret_type)) in function_table {
        st.insert(n, (false, tv.convert(TypeHint::Named(Type::Function(Box::new(arg_type), Box::new(ret_type))))));
    }

    check_statements(stmnts, &mut st, &mut tv)
}
