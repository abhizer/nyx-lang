use std::collections::HashMap;

use nyxc_ast::ast::{
    Expression, FunctionParameters, InflixOp, Literal, PrefixOp, Program, Statement, TypeInfo,
};

#[derive(Debug, Clone)]
pub struct Function<'a> {
    parameters: &'a [FunctionParameters],
    body: &'a Expression,
    ret: TypeInfo,
    name: &'a str,
}

#[derive(Debug, Default, Clone)]
pub struct Evaluator<'a> {
    functions: HashMap<&'a str, Function<'a>>,
    symbols: HashMap<&'a str, Literal>,
    break_flag: bool,
    break_value: Option<Literal>,
    continue_flag: bool,
}

impl<'a> Evaluator<'a> {
    pub fn eval(&mut self, ast: &'a Program) -> Vec<Literal> {
        let mut out = vec![];
        for stmt in ast.iter() {
            if let Some(val) = match stmt {
                Statement::Let(let_stmt) => {
                    let expr = self.eval_expr(&let_stmt.expression);
                    self.symbols.insert(&let_stmt.identifier, expr.unwrap());
                    None
                }
                Statement::Return(expr) => {
                    if let Some(expr_out) = self.eval_expr(expr) {
                        out.push(expr_out);
                    }
                    break;
                }
                Statement::Expr(expr) => self.eval_expr(&expr),
                Statement::Function {
                    parameters,
                    name,
                    ret_type,
                    body,
                } => {
                    self.functions.insert(
                        &name,
                        Function {
                            parameters: &parameters,
                            body,
                            ret: ret_type.to_owned().unwrap_or_default(),
                            name: &name,
                        },
                    );
                    None
                }
            } {
                out.push(val);
            };
        }
        out
    }

    fn eval_expr(&mut self, expr: &'a Expression) -> Option<Literal> {
        match expr {
            Expression::Identifier(ident) => {
                if let Some(ident) = &ident.get_identifier() {
                    return self.symbols.get(ident).cloned();
                }
                None
            }
            Expression::Literal(lit) => Some(lit.to_owned()),
            Expression::Reassignment {
                identifier,
                expression,
            } => {
                if let Some(expr) = self.eval_expr(expression) {
                    self.symbols.insert(&identifier, expr);
                }
                None
            }
            Expression::Prefix { operator, expr } => {
                let expr = self.eval_expr(expr)?;
                match (operator, expr) {
                    (PrefixOp::Minus, Literal::Int(i)) => Some(Literal::Int(-i)),
                    (PrefixOp::Minus, Literal::Float(f)) => Some(Literal::Float(-f)),
                    (PrefixOp::Bang, Literal::Bool(b)) => Some(Literal::Bool(!b)),
                    _ => unreachable!(), // unreachable thanks to our amazing type checker
                }
            }
            Expression::Inflix {
                operator,
                left,
                right,
            } => {
                let left = self.eval_expr(left)?;
                let right = self.eval_expr(right)?;

                Some(match operator {
                    InflixOp::Plus => left + right,
                    InflixOp::Minus => left - right,
                    InflixOp::EqEq => Literal::Bool(left == right),
                    InflixOp::Ne => Literal::Bool(left != right),
                    InflixOp::Le => Literal::Bool(left <= right),
                    InflixOp::Ge => Literal::Bool(left >= right),
                    InflixOp::Lt => Literal::Bool(left < right),
                    InflixOp::Gt => Literal::Bool(left > right),
                    InflixOp::Product => left * right,
                    InflixOp::Divide => left / right,
                    InflixOp::Modulo => left % right,
                    InflixOp::Power => left ^ right,
                    InflixOp::And => left & right,
                    InflixOp::Or => left | right,
                })
            }
            Expression::If {
                condition,
                consequence,
                alternate,
            } => {
                let condition = self.eval_expr(&condition)?;

                if matches!(condition, Literal::Bool(true)) {
                    self.eval_expr(&consequence)
                } else if let Some(alternate) = alternate {
                    self.eval_expr(&alternate)
                } else {
                    None
                }
            }
            Expression::Call {
                identifier,
                arguments,
            } => {
                let Some(ident) = &identifier.get_identifier() else {
                    return None;
                };

                let args: Vec<Literal> = arguments
                    .iter()
                    .filter_map(|arg| self.eval_expr(&arg))
                    .collect();

                if ident == &"print" {
                    args.iter().for_each(|arg| {
                        print!("{arg}");
                    });
                    println!()
                }

                let Some(func) = self.functions.get(ident).cloned() else {
                    return None;
                };

                let got = args.len();
                let expected = func.parameters.len();

                if got != expected {
                    unreachable!()
                }

                let mut new_evaluator = Self::default();
                new_evaluator.functions = self.functions.clone();

                args.iter()
                    .zip(func.parameters)
                    .for_each(|(param, param_name)| {
                        new_evaluator
                            .symbols
                            .insert(&param_name.ident, param.to_owned());
                    });

                let body = new_evaluator.eval_expr(func.body)?;

                if func.ret != TypeInfo::default() {
                    let body_ty: TypeInfo = body.clone().into();
                    if body_ty != func.ret {
                        unreachable!()
                    } else {
                        Some(body)
                    }
                } else {
                    None
                }
            }
            Expression::Continue => {
                self.continue_flag = true;
                None
            }
            Expression::Break(expr) => {
                let expr = self.eval_expr(expr);
                self.break_value = expr;
                self.break_flag = true;
                None
            }
            Expression::Loop(expr) => {
                loop {
                    self.eval_expr(expr);
                    if self.continue_flag {
                        self.continue_flag = false;
                        continue;
                    }

                    if self.break_flag {
                        break;
                    }
                }
                std::mem::take(&mut self.break_value)
            }
            Expression::Block(program) => {
                let ret = self.eval(program).last().cloned();
                ret
            }
            Expression::None => None,
        }
    }
}
