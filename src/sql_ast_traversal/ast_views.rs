use sqlparser::ast::ObjectName;
use sqlparser::ast::ObjectType;
use sqlparser::ast::Statement;

#[derive(Debug)]
pub struct DropStatementViewMutable<'a> {
    pub cascade: &'a mut bool,
    pub if_exists: &'a mut bool,
    pub names: &'a mut Vec<ObjectName>,
    pub object_type: &'a mut ObjectType,
    pub purge: &'a mut bool,
    pub restrict: &'a mut bool,
    pub temporary: &'a mut bool,
}

impl<'a> TryFrom<&'a mut Statement> for DropStatementViewMutable<'a> {
    type Error = String;

    fn try_from(statement: &'a mut Statement) -> Result<Self, Self::Error> {
        match statement {
            Statement::Drop {
                cascade,
                if_exists,
                names,
                object_type,
                purge,
                restrict,
                temporary,
            } => Ok(DropStatementViewMutable {
                cascade,
                if_exists,
                names,
                object_type,
                purge,
                restrict,
                temporary,
            }),
            _ => Err("Expected a Drop statement".to_string()),
        }
    }
}
