use sqlparser::ast::Ident;

pub fn extract_unary_identifier(
    identifiers: &Vec<Ident>,
    object_type: &str,
) -> Result<String, String> {
    if identifiers.len() != 1 {
        return Err(format!(
            "Expected 1 identifiers for the {} name, got: {:?}",
            object_type,
            identifiers
                .iter()
                .map(|x| x.to_string())
                .collect::<Vec<_>>()
        ));
    }

    Ok(identifiers[0].value.clone())
}

pub fn extract_binary_identifiers(
    identifiers: &Vec<Ident>,
    object_type: &str,
) -> Result<(String, String), String> {
    if identifiers.len() != 2 {
        return Err(format!(
            "Expected 2 identifiers for the {} name, got: {:?}",
            object_type,
            identifiers
                .iter()
                .map(|x| x.to_string())
                .collect::<Vec<_>>()
        ));
    }

    Ok((identifiers[0].value.clone(), identifiers[1].value.clone()))
}
