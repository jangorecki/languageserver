# languageserver (minimal fork)

A dependency-free [Language Server Protocol](https://microsoft.github.io/language-server-protocol/)
implementation for R, forked from
[REditorSupport/languageserver](https://github.com/REditorSupport/languageserver)
and cut down to the essentials: ~450 lines of base R plus ~240 lines of portable C.

* No package dependencies (only `utils` and `tools`, which ship with R).
* No OS dependencies: no subprocesses, sockets, file watchers or process monitors.
  The server runs in a single R process and talks JSON-RPC over stdin/stdout;
  JSON and message framing are implemented in `src/lsp.c`.

## Features

| LSP method | Notes |
|---|---|
| `textDocument/publishDiagnostics` | syntax errors from R's parser |
| `textDocument/completion` (+ `completionItem/resolve`) | workspace symbols, local variables, function arguments, keywords, exports of attached / `library()`ed / DESCRIPTION-imported packages, `pkg::` and `pkg:::`, package names in `library()` |
| `textDocument/hover` | signature plus roxygen comments (workspace) or Rd help (packages) |
| `textDocument/signatureHelp` | workspace and package functions, named and positional arguments |
| `textDocument/definition` | scope-aware, across the workspace |
| `textDocument/references`, `documentHighlight`, `rename` | scope-aware (function scopes, shadowing, `$`/`@` members excluded) |
| `textDocument/documentSymbol`, `workspace/symbol` | top-level definitions and `# Section ----` comments |
| `textDocument/foldingRange` | multi-line `{`/`(` blocks and sections |

Not included: formatting (use [Air](https://posit-dev.github.io/air/)) and lintr
diagnostics, since both need extra packages.

## Install and use

```r
remotes::install_github("<you>/languageserver@minimal")   # or: R CMD INSTALL .
```

Any LSP client can start it with `R --slave -e "languageserver::run()"`.
The [Zed](https://zed.dev) R extension already uses that command, so
installing this package in place of the CRAN one is all that is needed:

```json
{ "languages": { "R": { "language_servers": ["air", "r_language_server"] } } }
```

`languageserver::run(debug = TRUE)` logs requests and errors to stderr.

## Test

```sh
Rscript tests/smoke.R
```
