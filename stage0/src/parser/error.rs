use crate::ast::TextSpan;

#[derive(Debug, Clone)]
pub(super) struct ParseError {
    pub(super) span: TextSpan,
    pub(super) message: String,
    pub(super) help: Option<&'static str>,
}
