# LSP request handlers.

KEYWORDS <- c("if", "else", "repeat", "while", "function", "for", "in", "next", "break",
              "TRUE", "FALSE", "NULL", "Inf", "NaN", "NA", "NA_integer_", "NA_real_", "NA_character_")
SECTION <- "^\\s*#+\\s*(.*?)\\s*(----|====|####)[-=#]*\\s*$"

H <- list(
    initialize = function(p) {
        S$root <- if (!is.null(p$rootUri)) uri2path(p$rootUri) else p$rootPath
        list(capabilities = list(
            textDocumentSync = list(openClose = TRUE, change = 1),
            completionProvider = list(triggerCharacters = list(":"), resolveProvider = TRUE),
            signatureHelpProvider = list(triggerCharacters = list("(", ",")),
            hoverProvider = TRUE, definitionProvider = TRUE, referencesProvider = TRUE,
            documentHighlightProvider = TRUE, renameProvider = TRUE, documentSymbolProvider = TRUE,
            workspaceSymbolProvider = TRUE, foldingRangeProvider = TRUE),
            serverInfo = list(name = "languageserver", version = as.character(utils::packageVersion("languageserver"))))
    },
    initialized = function(p) index_workspace(),
    shutdown = function(p) NULL,
    exit = function(p) quit(save = "no"),
    "textDocument/didOpen" = function(p) set_doc(p$textDocument$uri, p$textDocument$text),
    "textDocument/didChange" = function(p) set_doc(p$textDocument$uri, p$contentChanges[[length(p$contentChanges)]]$text),
    "textDocument/didClose" = function(p) {
        u <- p$textDocument$uri
        f <- uri2path(u)
        notify("textDocument/publishDiagnostics", list(uri = u, diagnostics = list()))
        if (!is.null(S$root) && startsWith(f, S$root) && file.exists(f)) set_doc(u, read_file(f), FALSE)
        else rm(list = f, envir = S$docs)
    },
    "textDocument/completion" = function(p) completion(doc_at(p), p$position),
    "completionItem/resolve" = function(it) {
        v <- describe(it$label, if (is.null(it$data$pkg)) NA else it$data$pkg)
        if (!is.null(v)) it$documentation <- list(kind = "markdown", value = v)
        it
    },
    "textDocument/hover" = function(p) {
        d <- doc_at(p)
        x <- at(d, p$position)
        v <- if (nzchar(x$word)) describe(x$word, x$pkg, d, x$key)
        if (!is.null(v)) list(contents = list(kind = "markdown", value = v))
    },
    "textDocument/signatureHelp" = function(p) signature(doc_at(p), p$position),
    "textDocument/definition" = function(p) {
        d <- doc_at(p)
        x <- at(d, p$position)
        hit <- if (is.na(x$pkg) && nzchar(x$word)) find_def(d, x$word, x$key)
        if (!is.null(hit)) loc(hit$doc, hit$def$row)
    },
    "textDocument/references" = function(p) {
        r <- refs(doc_at(p), p$position)
        unlist(lapply(r, function(o) lapply(o$i, function(i) loc(o$doc, i))), recursive = FALSE)
    },
    "textDocument/documentHighlight" = function(p) {
        d <- doc_at(p)
        r <- Filter(function(o) identical(o$doc$uri, d$uri), refs(d, p$position))
        if (length(r)) lapply(r[[1]]$i, function(i) list(range = tok_range(d$pd, i)))
    },
    "textDocument/rename" = function(p) {
        r <- refs(doc_at(p), p$position, rename = TRUE)
        r <- Filter(function(o) length(o$i), r)
        if (any(vapply(r, function(o) isTRUE(o$doc$stale), NA))) stop("fix syntax errors before renaming")
        ch <- lapply(r, function(o) lapply(o$i, function(i) list(range = tok_range(o$doc$pd, i), newText = p$newName)))
        list(changes = setNames(ch, vapply(r, function(o) o$doc$uri, "")))
    },
    "textDocument/documentSymbol" = function(p) {
        d <- doc_at(p)
        s <- grep(SECTION, d$lines, perl = TRUE)
        c(symbols(d, d$defs[d$defs$scope == 0, ]),
          lapply(s, function(l) list(name = sub(SECTION, "\\1", d$lines[l], perl = TRUE), kind = 15,
                                     location = list(uri = d$uri, range = rng(l - 1, 0, l - 1, nchar(d$lines[l]))))))
    },
    "workspace/symbol" = function(p) {
        unlist(lapply(docs(), function(d) {
            g <- d$defs[d$defs$scope == 0, ]
            symbols(d, g[fuzzy(g$name, p$query), ])
        }), recursive = FALSE)
    },
    "textDocument/foldingRange" = function(p) {
        d <- doc_at(p)
        pd <- d$pd
        b <- unique(match(pd$parent[pd$token %in% c("'{'", "'('")], pd$id))
        b <- b[pd$l2[b] > pd$l1[b]]
        s <- grep(SECTION, d$lines, perl = TRUE)
        e <- c(s[-1] - 1, length(d$lines)) - 1
        r <- c(Map(function(a, z) list(startLine = a, endLine = z), pd$l1[b], pd$l2[b] - 1),
               Map(function(a, z) list(startLine = a, endLine = z), s - 1, e))
        r[!duplicated(vapply(r, function(x) x$startLine, 0))]
    }
)

doc_at <- function(p) {
    d <- S$docs[[uri2path(p$textDocument$uri)]]
    if (is.null(d)) stop("unknown document") else d
}

# Text around a position: identifier under the cursor, optional `pkg::` prefix.
at <- function(d, pos) {
    line <- c(d$lines, "")[pos$line + 1]
    if (is.na(line)) line <- ""
    cp <- utf8ToInt(line)
    k <- sum(cumsum(1 + (cp > 0xFFFF)) <= pos$character)  # UTF-16 -> chars
    pre <- substr(line, 1, k)
    w1 <- regmatches(pre, regexpr("[[:alnum:]._]*$", pre))
    post <- substring(line, k + 1)
    w2 <- regmatches(post, regexpr("^[[:alnum:]._]*", post))
    pk <- regmatches(pre, regexec("([[:alnum:]._]+):::?[[:alnum:]._]*$", pre))[[1]]
    list(line = line, pre = pre, prefix = w1, word = paste0(w1, w2), pkg = if (length(pk)) pk[2] else NA,
         key = pos$line * 1e5 + pos$character)
}

fuzzy <- function(x, q) grepl(paste0("^", paste0("\\Q", strsplit(q, "")[[1]], "\\E", collapse = ".*")), x, perl = TRUE, ignore.case = TRUE)

# Innermost definition of `name` visible at `key` in d, else a global one anywhere.
find_def <- function(d, name, key = 0) {
    if (!is.null(d)) {
        df <- d$defs[d$defs$name == name & d$defs$s1 <= key & d$defs$s2 >= key, ]
        if (nrow(df)) {
            df <- df[df$s1 == max(df$s1), ]
            before <- df[d$pd$k1[df$row] <= key, ]
            return(list(doc = d, def = if (nrow(before)) before[nrow(before), ] else df[1, ]))
        }
    }
    for (o in docs()) {
        g <- o$defs[o$defs$name == name & o$defs$scope == 0, ]
        if (nrow(g)) return(list(doc = o, def = g[1, ]))
    }
}

# Occurrences of the symbol at pos, respecting function scopes and shadowing.
refs <- function(d, pos, rename = FALSE) {
    x <- at(d, pos)
    if (!nzchar(x$word)) return(list())
    hit <- find_def(d, x$word, x$key)
    if (rename && is.null(hit)) stop("no definition of '", x$word, "' in the workspace")
    sc <- if (is.null(hit)) 0 else hit$def$scope
    ds <- if (sc != 0) list(hit$doc) else docs()
    lo <- if (sc != 0) hit$def$s1 else -Inf
    hi <- if (sc != 0) hit$def$s2 else Inf
    lapply(ds, function(o) {
        pd <- o$pd
        ok <- pd$text == x$word & pd$token %in% SYM & !pd$prev %in% c("'$'", "'@'") & pd$k1 >= lo & pd$k2 <= hi
        sh <- o$defs[o$defs$name == x$word & o$defs$scope != sc & o$defs$s1 >= lo & o$defs$s2 <= hi, ]
        for (j in seq_len(nrow(sh))) ok <- ok & !(pd$k1 >= sh$s1[j] & pd$k2 <= sh$s2[j])
        list(doc = o, i = which(ok))
    })
}

symbols <- function(d, defs) {
    lapply(seq_len(nrow(defs)), function(j) {
        e <- match(defs$expr[j], d$pd$id)
        list(name = defs$name[j], kind = if (is.na(defs$fun[j])) 13 else 12,
             location = list(uri = d$uri, range = tok_range(d$pd, e)))
    })
}

# Packages in scope: library()/require() calls in the document, DESCRIPTION
# imports, then the default attached packages.
doc_pkgs <- function(d) {
    m <- regmatches(d$lines, regexec("(?:library|require|requireNamespace)\\(\\s*[\"']?([[:alnum:].]+)", d$lines, perl = TRUE))
    unique(c(rev(vapply(Filter(length, m), `[`, "", 2)), S$imports, rev(getOption("defaultPackages")), "base"))
}

pkg_syms <- function(pkg, all = FALSE) {
    id <- paste0(pkg, all)
    if (is.null(S$pkgs[[id]])) S$pkgs[[id]] <- tryCatch({
        ns <- suppressMessages(loadNamespace(pkg))
        nm <- if (pkg == "base" || all) ls(ns, all.names = all) else
            c(getNamespaceExports(ns), ls(getNamespaceInfo(ns, "lazydata")))
        nm <- nm[!startsWith(nm, ".__")]
        data.frame(name = nm, fun = vapply(nm, function(x) is.function(get0(x, ns)), NA))
    }, error = function(e) data.frame(name = character(), fun = logical()))
    S$pkgs[[id]]
}

pkg_fun <- function(name, pkgs) {
    for (p in pkgs) if (name %in% pkg_syms(p)$name) return(list(pkg = p, f = get0(name, asNamespace(p))))
}

ws_fun <- function(hit) {
    if (!is.null(hit) && !is.na(hit$def$fun))
        eval(parse(text = utils::getParseText(hit$doc$pd, hit$def$fun))[[1]], baseenv())
}

# "name(a, b = 1)" with the UTF-16 offsets of each parameter as attribute.
fsig <- function(name, f) {
    a <- formals(args(f))
    def <- vapply(a, function(v) paste(deparse(v), collapse = " "), "")
    parts <- if (length(a)) paste0(names(a), ifelse(def == "", "", paste(" =", def)))
    s <- nchar(name) + 1 + c(0, cumsum(nchar(parts) + 2))[seq_along(parts)]
    structure(paste0(name, "(", paste(parts, collapse = ", "), ")"),
              params = Map(c, s, s + nchar(parts)), args = names(a))
}

describe <- function(name, pkg = NA, d = NULL, key = 0) {
    if (is.na(pkg) && !is.null(hit <- find_def(d, name, key))) {
        o <- hit$doc
        l <- o$pd$line1[match(hit$def$expr, o$pd$id)]
        f <- ws_fun(hit)
        head <- if (is.function(f)) fsig(name, f) else trimws(o$lines[l])
        cm <- 0
        while (l - cm > 1 && grepl("^\\s*#", o$lines[l - cm - 1])) cm <- cm + 1
        txt <- sub("^\\s*#+'?\\s?", "", o$lines[seq_len(cm) + l - cm - 1])
        txt <- sub("^@param\\s+(\\S+)", "* `\\1`", sub("^@return", "Returns:", txt))
        return(paste(c("```r", head, "```", txt[!grepl("^@", txt)]), collapse = "\n"))
    }
    x <- pkg_fun(name, if (is.na(pkg)) doc_pkgs(d) else pkg)
    if (is.null(x)) return(NULL)
    head <- if (is.function(x$f)) c("```r", fsig(name, x$f), "```") else paste0("`", name, "` {", x$pkg, "}")
    paste(c(head, rd_doc(name, x$pkg)), collapse = "\n")
}

rd_doc <- function(name, pkg) {
    h <- tryCatch(utils::help(name, package = (pkg)), error = function(e) NULL)
    if (!length(h)) return(NULL)
    rd <- utils:::.getHelpFile(h[1])
    txt <- utils::capture.output(tools::Rd2txt(rd, options = list(underline_titles = FALSE)))
    hd <- c(grep("^\\S.*:$", txt), length(txt) + 1)
    drop <- unlist(lapply(which(txt[hd] %in% c("Usage:", "Examples:", "See Also:")), function(j) hd[j]:(hd[j + 1] - 1)))
    body <- sub("^(\\S.*):$", "**\\1**", trimws(txt[-c(1, drop)]))
    paste(c(paste0("**", txt[1], "**"), body), collapse = "\n")
}

# Innermost unclosed `(` before the cursor: called function and argument index.
enclosing_call <- function(d, pos, x) {
    l <- pos$line + 1
    txt <- paste(c(if (l > 1) d$lines[max(1, l - 50):(l - 1)], x$pre), collapse = "\n")
    ch <- strsplit(txt, "")[[1]]
    st <- cm <- as <- integer()
    q <- ""
    esc <- com <- FALSE
    for (i in seq_along(ch)) {
        c <- ch[i]
        if (com) { com <- c != "\n"; next }
        if (nzchar(q)) { if (esc) esc <- FALSE else if (c == "\\") esc <- TRUE else if (c == q) q <- ""; next }
        switch(c,
            "'" = , "\"" = , "`" = q <- c,
            "#" = com <- TRUE,
            "(" = , "[" = , "{" = { st <- c(st, i); cm <- c(cm, 0L); as <- c(as, i) },
            ")" = , "]" = , "}" = { n <- length(st); st <- st[-n]; cm <- cm[-n]; as <- as[-n] },
            "," = if (length(st)) { n <- length(st); cm[n] <- cm[n] + 1L; as[n] <- i })
    }
    n <- length(st)
    if (!n || ch[st[n]] != "(") return(NULL)
    before <- paste(ch[seq_len(st[n] - 1)], collapse = "")
    fn <- regmatches(before, regexec("(?:([[:alnum:]._]+):::?)?([[:alnum:]._]+)\\s*$", before, perl = TRUE))[[1]]
    if (!length(fn) || fn[3] %in% KEYWORDS) return(NULL)
    arg <- paste(ch[-seq_len(as[n])], collapse = "")
    list(fn = fn[3], pkg = if (nzchar(fn[2])) fn[2] else NA, commas = cm[n],
         named = regmatches(arg, regexec("^\\s*([[:alnum:]._]+)\\s*=($|[^=])", arg))[[1]][2])
}

call_fun <- function(d, cl, key) {
    f <- if (is.na(cl$pkg)) ws_fun(find_def(d, cl$fn, key))
    if (is.null(f)) f <- pkg_fun(cl$fn, if (is.na(cl$pkg)) doc_pkgs(d) else cl$pkg)$f
    if (is.function(f)) fsig(cl$fn, f)
}

signature <- function(d, pos) {
    x <- at(d, pos)
    cl <- enclosing_call(d, pos, x)
    sig <- if (!is.null(cl)) call_fun(d, cl, x$key)
    if (is.null(sig)) return(NULL)
    a <- attr(sig, "args")
    act <- match(cl$named, a)
    if (is.na(act)) act <- min(cl$commas + 1, match("...", a), length(a), na.rm = TRUE)
    list(signatures = list(list(label = as.vector(sig), parameters = lapply(attr(sig, "params"), function(o) list(label = o)))),
         activeSignature = 0, activeParameter = act - 1)
}

completion <- function(d, pos) {
    x <- at(d, pos)
    if (grepl("[$@][[:alnum:]._]*$", x$pre)) return(list())
    ns <- regmatches(x$pre, regexec("([[:alnum:]._]+)(:::?)[[:alnum:]._]*$", x$pre))[[1]]
    cand <- function(label, kind, rank, pkg = NA) if (length(label)) data.frame(label, kind, rank, pkg)
    if (length(ns)) {
        s <- pkg_syms(ns[2], ns[3] == ":::")
        cs <- cand(s$name, ifelse(s$fun, 3, 6), 1, ns[2])
    } else {
        cl <- enclosing_call(d, pos, x)
        sig <- if (!is.null(cl)) call_fun(d, cl, x$key)
        if (!nzchar(x$prefix) && is.null(sig)) return(list())
        g <- do.call(rbind, lapply(docs(), function(o) o$defs[o$defs$scope == 0, ]))
        loc <- d$defs[d$defs$scope != 0 & d$defs$s1 <= x$key & d$defs$s2 >= x$key, ]
        ws <- rbind(loc, g)
        cs <- do.call(rbind, c(
            list(cand(setdiff(attr(sig, "args"), "..."), 5, 0),
                 cand(ws$name, ifelse(is.na(ws$fun), 6, 3), 1),
                 cand(KEYWORDS, 14, 2)),
            if (!is.null(cl) && cl$fn %in% c("library", "require", "requireNamespace"))
                list(cand(.packages(all.available = TRUE), 9, 0)),
            lapply(doc_pkgs(d), function(p) { s <- pkg_syms(p); cand(s$name, ifelse(s$fun, 3, 6), 3, p) })))
        if (!is.null(sig)) cs$label[cs$rank == 0 & cs$kind == 5] <- paste(cs$label[cs$rank == 0 & cs$kind == 5], "= ")
    }
    if (is.null(cs)) return(list())
    cs <- cs[!duplicated(cs$label) & fuzzy(cs$label, x$prefix), ]
    cs <- cs[order(cs$rank), ]
    items <- lapply(seq_len(min(nrow(cs), 500)), function(i) list(
        label = cs$label[i], kind = cs$kind[i], sortText = paste0(cs$rank[i], cs$label[i]),
        detail = if (is.na(cs$pkg[i])) "" else paste0("{", cs$pkg[i], "}"), data = list(pkg = cs$pkg[i])))
    list(isIncomplete = nrow(cs) > 500, items = items)
}
