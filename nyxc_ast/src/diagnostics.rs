use super::Token;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum DiagnosticLevel {
    Hint,
    Warning,
    Error,
}

#[derive(Debug, Clone)]
pub struct Diagnostics {
    pub expected_token: Token,
    pub got_token: Option<Token>,
    pub level: DiagnosticLevel,
    pub detail: Option<String>,
}

impl Diagnostics {
    pub fn new(
        expected_token: Token,
        got_token: Option<Token>,
        level: DiagnosticLevel,
        detail: Option<String>,
    ) -> Self {
        Self {
            expected_token,
            got_token,
            level,
            detail,
        }
    }
}
