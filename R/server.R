# Protocol loop, document store and parsing. Base R only.

S <- new.env()  # server state

# Entry point: speak LSP on stdin/stdout until EOF or `exit`.
run <- function(debug = FALSE, ...) {
    if (!l10n_info()[["UTF-8"]]) suppressWarnings(Sys.setlocale("LC_CTYPE", "C.UTF-8"))
    S$debug <- debug
    S$docs <- new.env()
    S$pkgs <- new.env()
    sink(stderr())  # stray R output must never reach the protocol stream
    on.exit(sink())
    repeat {
        m <- .Call(lsp_read)
        if (is.null(m)) break
        if (debug) message("<- ", m$method)
        tryCatch(suppressWarnings(dispatch(m)), error = function(e) if (debug) message("error: ", conditionMessage(e)))
    }
}

dispatch <- function(m) {
    h <- if (!is.null(m$method)) H[[m$method]]
    if (is.null(m$id)) return(if (!is.null(h)) h(m$params))
    if (is.null(m$method)) return()  # reply to a server request
    res <- if (is.null(h)) list(error = list(code = -32601, message = paste("unsupported:", m$method)))
           else tryCatch(list(result = h(m$params)),
                         error = function(e) list(error = list(code = -32603, message = conditionMessage(e))))
    send(c(list(jsonrpc = "2.0", id = m$id), res))
}

send <- function(x) .Call(lsp_write, x)
notify <- function(method, params) send(list(jsonrpc = "2.0", method = method, params = params))

uri2path <- function(u) sub("^/([A-Za-z]:)", "\\1", utils::URLdecode(sub("^file://", "", u)))
path2uri <- function(p) paste0("file://", if (!startsWith(p, "/")) "/", utils::URLencode(p))
read_file <- function(f) paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

set_doc <- function(uri, text, open = TRUE) {
    d <- parse_doc(text)
    d$uri <- uri
    d$open <- open
    old <- S$docs[[uri2path(uri)]]
    if (is.null(d$pd)) {  # syntax error: keep the last good parse for navigation
        base <- if (is.null(old$pd)) parse_doc("") else old
        d$pd <- base$pd
        d$defs <- base$defs
        d$stale <- TRUE
    }
    assign(uri2path(uri), d, envir = S$docs)
    if (open) publish_diagnostics(d)
}

publish_diagnostics <- function(d) {
    dg <- list()
    if (!is.null(d$err)) {
        m <- regmatches(d$err, regexec("^<text>:(\\d+):(\\d+): ([^\n]*)", d$err))[[1]]
        if (length(m)) {
            l <- as.integer(m[2])
            c <- .Call(cols16, d$lines, l, as.integer(m[3]), FALSE)
            dg <- list(list(range = rng(l - 1, c, l - 1, c + 1), severity = 1, source = "R", message = m[4]))
        }
    }
    notify("textDocument/publishDiagnostics", list(uri = d$uri, diagnostics = dg))
}

rng <- function(l1, c1, l2, c2) list(start = list(line = l1, character = c1), end = list(line = l2, character = c2))
tok_range <- function(pd, i) rng(pd$l1[i], pd$c1[i], pd$l2[i], pd$c2[i])
loc <- function(d, i) list(uri = d$uri, range = tok_range(d$pd, i))

parse_doc <- function(text) {
    lines <- src <- sub("\r$", "", strsplit(text, "\n", fixed = TRUE)[[1]])
    err <- NULL
    for (try in 1:5) {  # on syntax errors, blank the broken top-level block and retry
        ex <- tryCatch(parse(text = src, keep.source = TRUE), error = conditionMessage)
        if (!is.character(ex)) break
        err <- c(err, ex)[1]
        l <- as.integer(sub("^<text>:(\\d+):.*", "\\1", ex))
        if (is.na(l)) break
        top <- grep("^[^\\s#)}\\]]", src, perl = TRUE)
        src[max(1, top[top <= l]):(min(top[top > l], length(src) + 1) - 1)] <- ""
    }
    if (is.character(ex)) return(list(lines = lines, err = err))
    pd <- utils::getParseData(ex)
    if (is.null(pd)) pd <- utils::getParseData(parse(text = "NULL", keep.source = TRUE))[0, ]
    pd$l1 <- pd$line1 - 1L
    pd$l2 <- pd$line2 - 1L
    pd$c1 <- .Call(cols16, lines, pd$line1, pd$col1, FALSE)
    pd$c2 <- .Call(cols16, lines, pd$line2, pd$col2, TRUE)
    pd$k1 <- pd$l1 * 1e5 + pd$c1  # sortable position keys
    pd$k2 <- pd$l2 * 1e5 + pd$c2
    t <- which(pd$terminal)
    pd$prev <- rep("", nrow(pd))
    pd$prev[t] <- c("", pd$token[t][-length(t)])
    list(lines = lines, pd = pd, defs = find_defs(pd), err = err, stale = !is.null(err))
}

FUN <- c("FUNCTION", "'\\\\'")
SYM <- c("SYMBOL", "SYMBOL_FUNCTION_CALL", "SYMBOL_FORMALS")

# Definitions: `x <- `, `x = `, `-> x` and function formals. scope is the id of
# the enclosing function expression (0 = global); fun is the id of a function rhs.
find_defs <- function(pd) {
    n <- max(c(pd$id, 1))
    kids <- split(seq_len(nrow(pd)), factor(pd$parent, seq_len(n)))
    kid <- function(id) kids[[id]]
    par <- isf <- integer(n)
    par[pd$id] <- pd$parent
    isf[pd$parent[pd$token %in% FUN]] <- 1L
    scope <- function(id) { while (id > 0 && !isf[id]) id <- par[id]; max(id, 0) }
    a <- which(pd$token %in% c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN"))
    right <- pd$token[a] == "RIGHT_ASSIGN"
    lhs <- rhs <- integer(length(a))
    for (i in seq_along(a)) {
        s <- kid(pd$parent[a[i]])
        lhs[i] <- s[if (right[i]) length(s) else 1]
        rhs[i] <- s[if (right[i]) 1 else length(s)]
    }
    tgt <- vapply(lhs, function(j) { k <- kid(pd$id[j]); if (length(k) == 1 && pd$token[k] == "SYMBOL") k else NA_integer_ }, 1L)
    isfun <- vapply(rhs, function(j) any(pd$token[kid(pd$id[j])] %in% FUN), NA)
    ok <- !is.na(tgt)
    sup <- pd$text[a] %in% c("<<-", "->>")
    fm <- which(pd$token == "SYMBOL_FORMALS")
    defs <- data.frame(
        name = pd$text[c(tgt[ok], fm)],
        row = c(tgt[ok], fm),
        fun = c(ifelse(isfun, pd$id[rhs], NA)[ok], rep(NA, length(fm))),
        expr = c(pd$parent[a][ok], pd$parent[fm]),
        scope = c(vapply(pd$parent[a][ok], scope, 0) * !sup[ok], pd$parent[fm]))
    s <- match(defs$scope, pd$id)
    defs$s1 <- ifelse(defs$scope == 0, -Inf, pd$k1[s])
    defs$s2 <- ifelse(defs$scope == 0, Inf, pd$k2[s])
    defs[order(pd$k1[defs$row]), ]
}

index_workspace <- function() {
    if (is.null(S$root) || !dir.exists(S$root)) return()
    fs <- list.files(S$root, "\\.[Rr]$", recursive = TRUE, full.names = TRUE)
    for (f in utils::head(fs[!grepl("/(renv|packrat)/", fs)], 2000))
        if (is.null(S$docs[[f]])) set_doc(path2uri(f), read_file(f), open = FALSE)
    desc <- file.path(S$root, "DESCRIPTION")
    if (file.exists(desc)) {
        x <- read.dcf(desc, c("Depends", "Imports"))
        p <- trimws(sub("\\(.*", "", unlist(strsplit(x[!is.na(x)], ","))))
        S$imports <- setdiff(p, c("R", ""))
    }
}

docs <- function() unname(as.list(S$docs))
