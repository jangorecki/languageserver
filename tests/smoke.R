# End-to-end check: pipe a scripted LSP session through a fresh R process.
msg <- function(id, method, params) {
    body <- sprintf('{"jsonrpc":"2.0",%s"method":"%s","params":%s}',
                    if (is.na(id)) "" else sprintf('"id":%d,', id), method, params)
    paste0("Content-Length: ", nchar(body, "bytes"), "\r\n\r\n", body)
}
uri <- "file:///tmp/smoke.R"
pos <- function(l, c) sprintf('{"textDocument":{"uri":"%s"},"position":{"line":%d,"character":%d}}', uri, l, c)
text <- 'add2 <- function(x, y = 1) x + y\\nadd2(1, y = 2)\\npaste0(\\"a\\", \\"😀\\"); add2\\nbad <- ('
input <- tempfile()
writeLines(c(
    msg(1, "initialize", '{"rootUri":null,"capabilities":{}}'),
    msg(NA, "textDocument/didOpen", sprintf('{"textDocument":{"uri":"%s","languageId":"r","version":1,"text":"%s"}}', uri, text)),
    msg(2, "textDocument/hover", pos(1, 1)),
    msg(3, "textDocument/definition", pos(2, 19)),
    msg(4, "textDocument/signatureHelp", pos(1, 9)),
    msg(5, "textDocument/completion", pos(1, 3)),
    msg(6, "textDocument/references", pos(0, 0)),
    msg(7, "shutdown", "null"), msg(NA, "exit", "null")), input, sep = "", useBytes = TRUE)
out <- system2(file.path(R.home("bin"), "R"), c("--slave", "-e", shQuote("languageserver::run()")),
               stdin = input, stdout = TRUE)
out <- paste(out, collapse = "\n")
expect <- c('"id":2,"result":{"contents":{"kind":"markdown","value":"```r\\nadd2(x, y = 1)',
            '"id":3,"result":{"uri":"file:///tmp/smoke.R","range":{"start":{"line":0,"character":0}',
            '"activeParameter":1', '"label":"add2"', '"start":{"line":2,"character":19}',
            '"message":"unexpected end of input"', '"id":7,"result":null')
missing <- expect[!vapply(expect, grepl, NA, out, fixed = TRUE)]
if (length(missing)) { cat(out, "\n"); stop("missing from server output:\n", paste(missing, collapse = "\n")) }
