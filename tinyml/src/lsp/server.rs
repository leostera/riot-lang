//! Tower-LSP backend for TinyML.

use std::{collections::HashMap, sync::Arc};

use tokio::sync::RwLock;
use tower_lsp::{
    Client, LanguageServer, LspService, Server,
    jsonrpc::Result,
    lsp_types::{
        DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
        DidSaveTextDocumentParams, Hover, HoverParams, HoverProviderCapability, InitializeParams,
        InitializeResult, InlayHintParams, InlayHintServerCapabilities, MessageType, OneOf,
        ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind, Url,
    },
};

use crate::{
    checker::{self, ModuleSummary},
    parser::{self, lexer::Lexer, parse::Parser},
};

use super::{diagnostics, type_hints};

#[derive(Debug, Clone)]
struct Document {
    source: String,
    checked: Option<CheckedDocument>,
}

#[derive(Debug, Clone)]
struct CheckedDocument {
    summary: ModuleSummary,
}

#[derive(Debug, Clone)]
pub struct CheckedSource {
    pub ast: Option<parser::Ast>,
    pub summary: Option<ModuleSummary>,
    pub diagnostics: Vec<tower_lsp::lsp_types::Diagnostic>,
}

pub fn check_source(source: &str) -> CheckedSource {
    let lexer = match Lexer::new(source) {
        Ok(lexer) => lexer,
        Err(error) => {
            let start = usize::from(error.span.offset());
            let len = error.span.len();
            return CheckedSource {
                ast: None,
                summary: None,
                diagnostics: vec![diagnostics::span_diagnostic(
                    source,
                    crate::parser::lexer::Span::new(start, start + len),
                    error.to_string(),
                )],
            };
        }
    };

    let ast = match Parser::new(lexer).parse_module() {
        Ok(ast) => ast,
        Err(error) => {
            return CheckedSource {
                ast: None,
                summary: None,
                diagnostics: vec![diagnostics::parse_diagnostic(source, &error)],
            };
        }
    };

    let summary = checker::check_module(&ast);
    let diagnostics = diagnostics::checker_diagnostics(source, &summary.diagnostics);
    CheckedSource {
        ast: Some(ast),
        summary: Some(summary),
        diagnostics,
    }
}

struct Backend {
    client: Client,
    documents: Arc<RwLock<HashMap<Url, Document>>>,
}

impl Backend {
    fn new(client: Client) -> Self {
        Self {
            client,
            documents: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    async fn check_and_publish(&self, uri: Url, source: String) {
        let checked = check_source(&source);
        let document = Document {
            source,
            checked: checked.summary.map(|summary| CheckedDocument { summary }),
        };

        self.documents.write().await.insert(uri.clone(), document);
        self.client
            .publish_diagnostics(uri, checked.diagnostics, None)
            .await;
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                inlay_hint_provider: Some(OneOf::Right(InlayHintServerCapabilities::Options(
                    Default::default(),
                ))),
                ..ServerCapabilities::default()
            },
            server_info: None,
        })
    }

    async fn initialized(&self, _: tower_lsp::lsp_types::InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "TinyML language server initialized")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.check_and_publish(params.text_document.uri, params.text_document.text)
            .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let Some(change) = params.content_changes.into_iter().last() else {
            return;
        };
        self.check_and_publish(params.text_document.uri, change.text)
            .await;
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        if let Some(text) = params.text {
            self.check_and_publish(params.text_document.uri, text).await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.documents
            .write()
            .await
            .remove(&params.text_document.uri);
        self.client
            .publish_diagnostics(params.text_document.uri, Vec::new(), None)
            .await;
    }

    async fn inlay_hint(
        &self,
        params: InlayHintParams,
    ) -> Result<Option<Vec<tower_lsp::lsp_types::InlayHint>>> {
        let uri = params.text_document.uri;
        let documents = self.documents.read().await;
        let Some(document) = documents.get(&uri) else {
            return Ok(None);
        };
        let Some(checked) = &document.checked else {
            return Ok(Some(Vec::new()));
        };
        let hints = type_hints::inlay_hints(&document.source, &checked.summary)
            .into_iter()
            .filter(|hint| type_hints::range_contains(&params.range, hint.position))
            .collect();
        Ok(Some(hints))
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let documents = self.documents.read().await;
        let Some(document) = documents.get(&uri) else {
            return Ok(None);
        };
        let Some(checked) = &document.checked else {
            return Ok(None);
        };
        Ok(type_hints::hover(
            &document.source,
            &checked.summary,
            position,
        ))
    }
}

pub fn run_stdio() -> miette::Result<()> {
    let runtime = tokio::runtime::Runtime::new().map_err(|error| miette::miette!(error))?;
    runtime.block_on(async {
        let stdin = tokio::io::stdin();
        let stdout = tokio::io::stdout();
        let (service, socket) = LspService::new(Backend::new);
        Server::new(stdin, stdout, socket).serve(service).await;
    });
    Ok(())
}
