use nyxc_ast::ast::{
    Expression, FunctionParameters, InflixOp, Literal, PrefixOp, Program, Statement, TypeInfo,
};
use nyxc_ast::InnerSymbol;
use std::collections::HashMap;

use std::ops::Deref;

#[derive(Debug, Clone)]
pub struct FunctionTypeInfo<'a> {
    parameters: &'a [FunctionParameters],
    ret: Option<TypeInfo>,
}

#[derive(Debug, Default, Clone)]
pub struct TypeChecker<'a> {
    symbol_table: HashMap<&'a str, TypeInfo>,
    fn_table: HashMap<&'a str, FunctionTypeInfo<'a>>,
}

impl<'a> TypeChecker<'a> {
    fn check_expr(&self, expr: &Expression) -> Result<TypeInfo, String> {
        match expr {
            Expression::Identifier(symbol) => match symbol.get_identifier() {
                Some(ident) => match self.symbol_table.get(ident) {
                    Some(ty) => Ok(ty.clone()),
                    None => Err(format!("Unable to find type for identifier: {}", ident)),
                },
                None => Err("Expected an identifier, found a keyword".to_string()),
            },
            Expression::Literal(literal) => Ok(literal.into()),
            Expression::Reassignment {
                identifier,
                expression,
            } => {
                let expr_ty = self.check_expr(expression)?;
                match self.symbol_table.get(identifier.deref().as_str()) {
                    Some(ty) => {
                        if ty == &expr_ty {
                            Ok(expr_ty)
                        } else {
                            Err(format!(
                                "Type mismatch: expected {}, found {}",
                                ty.ty.deref(),
                                expr_ty.ty.deref()
                            ))
                        }
                    }
                    None => Err(format!("Undeclared identifier: {}", identifier.deref())),
                }
            }

            Expression::Prefix { operator, expr } => {
                let expr_ty = self.check_expr(expr)?;
                match operator {
                    PrefixOp::Minus if matches!(expr_ty.ty.deref().as_str(), "i64" | "f64") => {
                        Ok(expr_ty)
                    }
                    PrefixOp::Bang if matches!(expr_ty.ty.deref().as_str(), "bool") => Ok(expr_ty),
                    _ => Err(format!("Invalid operator for type {}", expr_ty.ty.deref())),
                }
            }

            Expression::Inflix {
                operator,
                left,
                right,
            } => {
                let left_ty = self.check_expr(left)?;
                let right_ty = self.check_expr(right)?;

                if left_ty != right_ty {
                    return Err(format!(
                        "Type mismatch: {} and {}",
                        left_ty.ty.deref(),
                        right_ty.ty.deref()
                    ));
                }

                match operator {
                    InflixOp::Plus
                    | InflixOp::Minus
                    | InflixOp::Product
                    | InflixOp::Divide
                    | InflixOp::Modulo
                    | InflixOp::Power
                        if matches!(left_ty.ty.deref().as_str(), "i64" | "f64") =>
                    {
                        Ok(left_ty)
                    }
                    InflixOp::Le | InflixOp::Ge | InflixOp::Lt | InflixOp::Gt
                        if matches!(left_ty.ty.deref().as_str(), "i64" | "f64") =>
                    {
                        Ok(TypeInfo {
                            ty: InnerSymbol::new("bool"),
                        })
                    }
                    InflixOp::EqEq | InflixOp::Ne
                        if matches!(
                            left_ty.ty.deref().as_str(),
                            "i64" | "f64" | "bool" | "string"
                        ) =>
                    {
                        Ok(TypeInfo {
                            ty: InnerSymbol::new("bool"),
                        })
                    }
                    InflixOp::And | InflixOp::Or
                        if matches!(left_ty.ty.deref().as_str(), "bool") =>
                    {
                        Ok(TypeInfo {
                            ty: InnerSymbol::new("bool"),
                        })
                    }
                    InflixOp::Plus if matches!(left_ty.ty.deref().as_str(), "string") => {
                        Ok(left_ty)
                    }
                    InflixOp::Power if matches!(left_ty.ty.deref().as_str(), "bool") => Ok(left_ty),
                    _ => Err(format!(
                        "Infix operation {operator} cannot be performed between types {} and {}",
                        left_ty.ty.deref(),
                        right_ty.ty.deref()
                    )),
                }
            }
            Expression::Call {
                identifier,
                arguments,
            } => {
                let fn_name = identifier.get_identifier().unwrap();
                match self.fn_table.get(fn_name) {
                    Some(fn_type_info) => {
                        let expected_params_len = fn_type_info.parameters.len();
                        let got_params_len = arguments.len();
                        if got_params_len != expected_params_len {
                            return Err(format!("Function {fn_name} expects {expected_params_len} parameters but got: {got_params_len} parameters"));
                        }

                        for (idx, (param, expected_param_type)) in arguments
                            .iter()
                            .zip(fn_type_info.parameters.iter())
                            .enumerate()
                        {
                            let param_ty = self.check_expr(param)?;
                            if param_ty != expected_param_type.ty {
                                return Err(format!("Function {fn_name} expects expression of type {} as parameter {} but got: {}", expected_param_type.ty.ty.deref().as_str(), idx + 1, param_ty.ty.deref().as_str()));
                            }
                        }

                        let x = fn_type_info.ret.clone().unwrap_or_default();
                        Ok(x)
                    }
                    None => {
                        if fn_name == "print" {
                            Ok(Default::default())
                        } else {
                            Err(format!("Call to an undefined function: {fn_name}"))
                        }
                    }
                }
            }
            Expression::Block(program) => {
                let mut new_tc = self.clone();
                let p_ty = new_tc.check_program(program)?;

                if let Some(last) = program.last() {
                    match last {
                        Statement::Return(expr) => {
                            return new_tc.check_expr(expr);
                        }
                        _ => {}
                    }
                }

                Ok(p_ty)
            }

            Expression::Break(expr) => self.check_expr(&*expr),

            Expression::If {
                condition,
                consequence,
                alternate,
            } => {
                if {
                    TypeInfo {
                        ty: InnerSymbol::new("bool"),
                    }
                } != self.check_expr(&condition)?
                {
                    return Err(format!("The if condition doesn't evaluate to a boolean"));
                }

                let consequence_ty = self.check_expr(&consequence)?;

                let alt_ty = if let Some(alt) = alternate {
                    self.check_expr(&alt)?
                } else {
                    TypeInfo::default()
                };

                if alternate.is_none() && consequence_ty != TypeInfo::default() {
                    return Err(format!("the consequence block of if statement returns: {} but there is no acompanying else block", consequence_ty.ty.deref()));
                } else if alt_ty != consequence_ty {
                    return Err(format!("the consequence block has return type: {} but the alternate block has return type: {}", consequence_ty.ty.deref(), alt_ty.ty.deref()));
                } else {
                    Ok(consequence_ty)
                }
            }

            Expression::Loop(expr) => self.check_expr(&expr),

            _ => Ok(TypeInfo::default()),
        }
    }

    fn check_stmt(&mut self, stmt: &'a Statement) -> Result<TypeInfo, String> {
        match stmt {
            Statement::Let(let_stmt) => {
                let ty = match &let_stmt.ty {
                    Some(ty) => ty.clone(),
                    None => self.check_expr(&let_stmt.expression)?,
                };
                self.symbol_table
                    .insert(let_stmt.identifier.deref().as_str(), ty);
                Ok(TypeInfo::default())
            }
            Statement::Return(expr) => self.check_expr(expr),
            Statement::Expr(expr) => self.check_expr(expr),
            Statement::Function {
                ret_type,
                parameters,
                name,
                body,
            } => {
                self.fn_table.insert(
                    name.deref().as_str(),
                    FunctionTypeInfo {
                        parameters: &parameters,
                        ret: ret_type.to_owned(),
                    },
                );

                let mut inner_typechecker = TypeChecker::default();
                parameters.iter().for_each(|e| {
                    inner_typechecker
                        .symbol_table
                        .insert(&e.ident.deref().as_str(), e.ty.clone());
                });

                inner_typechecker
                    .fn_table
                    .extend(self.fn_table.clone().into_iter());

                let body_ty = inner_typechecker.check_expr(body)?;

                let ret_ty = ret_type.clone().unwrap_or_default();

                if ret_ty != body_ty {
                    return Err(format!(
                        "function {} expects return type {}, got: {}",
                        &name.as_str(),
                        &ret_ty.ty.as_str(),
                        body_ty.ty.as_str(),
                    ));
                }

                Ok(ret_type.clone().unwrap_or_default())
            }
        }
    }

    pub fn check_program(&mut self, program: &'a Program) -> Result<TypeInfo, String> {
        let mut ty_info = None;
        for stmt in program {
            ty_info = Some(self.check_stmt(stmt)?);
        }
        Ok(ty_info.unwrap_or_default())
    }
}

#[cfg(test)]
mod tests {
    use nyxc_ast::{
        ast::{FunctionParameters, Let},
        Symbol,
    };

    use super::*;

    #[test]
    fn test_valid_program() {
        let mut type_checker = TypeChecker::default();
        let program = vec![
            Statement::Let(Let {
                mutable: false,
                expression: Expression::Literal(Literal::Int(5)),
                identifier: InnerSymbol::new("x"),
                ty: None,
            }),
            Statement::Let(Let {
                mutable: false,
                expression: Expression::Inflix {
                    operator: InflixOp::Plus,
                    left: Box::new(Expression::Identifier(Symbol::new("x"))),
                    right: Box::new(Expression::Literal(Literal::Int(3))),
                },
                identifier: InnerSymbol::new("y"),
                ty: None,
            }),
            Statement::Return(Expression::Identifier(Symbol::new("y"))),
        ];

        assert!(type_checker.check_program(&program).is_ok());
    }

    // #[test]
    // fn test_function_call() {
    //     let mut type_checker = TypeChecker::default();

    //     let program = vec![
    //         Statement::Function {
    //             parameters: vec![
    //                 FunctionParameters {
    //                     ident: InnerSymbol::new("a"),
    //                     ty: TypeInfo {
    //                         ty: InnerSymbol::new("i64"),
    //                     },
    //                 },
    //                 FunctionParameters {
    //                     ident: InnerSymbol::new("b"),
    //                     ty: TypeInfo {
    //                         ty: InnerSymbol::new("i64"),
    //                     },
    //                 },
    //             ],
    //             name: InnerSymbol::new("add"),
    //             ret_type: Some(TypeInfo {
    //                 ty: InnerSymbol::new("i64"),
    //             }),
    //             body: Expression::Inflix {
    //                 operator: InflixOp::Plus,
    //                 left: Box::new(Expression::Identifier(Symbol::Symbol(InnerSymbol::new(
    //                     "a",
    //                 )))),
    //                 right: Box::new(Expression::Identifier(Symbol::Symbol(InnerSymbol::new(
    //                     "b",
    //                 )))),
    //             },
    //         },
    //         Statement::Let(Let {
    //             mutable: false,
    //             expression: Expression::Call {
    //                 identifier: Symbol::Symbol(InnerSymbol::new("add")),
    //                 arguments: vec![
    //                     Expression::Literal(Literal::Int(5)),
    //                     Expression::Literal(Literal::Int(3)),
    //                 ],
    //             },
    //             identifier: InnerSymbol::new("result"),
    //             ty: None,
    //         }),
    //         Statement::Let(Let {
    //             mutable: false,
    //             expression: Expression::Inflix {
    //                 operator: InflixOp::Plus,
    //                 left: Box::new(Expression::Identifier(Symbol::Symbol(InnerSymbol::new(
    //                     "result",
    //                 )))),
    //                 right: Box::new(Expression::Literal(Literal::Float(3.14))),
    //             },
    //             identifier: InnerSymbol::new("final_result"),
    //             ty: None,
    //         }),
    //         Statement::Return(Expression::Identifier(Symbol::Symbol(InnerSymbol::new(
    //             "final_result",
    //         )))),
    //     ];

    //     type_checker.check_program(&program).unwrap();
    // }
}
