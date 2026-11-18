/* JSON-RPC over stdio for the R language server: framing, JSON <-> R values,
 * and R parser columns -> LSP UTF-16 offsets. Portable C99, no dependencies. */
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef _WIN32
#include <fcntl.h>
#include <io.h>
#endif

/* ---- JSON -> R: objects become named lists, arrays unnamed lists ---- */
typedef struct { const char *s, *e; } P;

static void ws(P *p) { while (p->s < p->e && (*p->s == ' ' || *p->s == '\t' || *p->s == '\r' || *p->s == '\n')) p->s++; }
static void expect(P *p, char c) { ws(p); if (p->s >= p->e || *p->s != c) error("invalid JSON"); p->s++; }

static unsigned hex4(P *p) {
    char t[5] = {0};
    if (p->e - p->s < 4) error("invalid JSON escape");
    memcpy(t, p->s, 4); p->s += 4;
    return (unsigned) strtoul(t, NULL, 16);
}

static char *put8(char *o, unsigned c) {
    if (c < 0x80) *o++ = (char) c;
    else if (c < 0x800) { *o++ = (char) (0xC0 | c >> 6); *o++ = (char) (0x80 | (c & 0x3F)); }
    else if (c < 0x10000) { *o++ = (char) (0xE0 | c >> 12); *o++ = (char) (0x80 | (c >> 6 & 0x3F)); *o++ = (char) (0x80 | (c & 0x3F)); }
    else { *o++ = (char) (0xF0 | c >> 18); *o++ = (char) (0x80 | (c >> 12 & 0x3F)); *o++ = (char) (0x80 | (c >> 6 & 0x3F)); *o++ = (char) (0x80 | (c & 0x3F)); }
    return o;
}

static SEXP str(P *p) {
    const char *q = ++p->s;
    while (q < p->e && *q != '"') q += (*q == '\\') ? 2 : 1;
    char *buf = R_alloc((size_t) (q - p->s) + 1, 1), *o = buf;
    while (p->s < q) {
        char c = *p->s++;
        if (c != '\\') { *o++ = c; continue; }
        switch (c = *p->s++) {
        case 'n': *o++ = '\n'; break;
        case 't': *o++ = '\t'; break;
        case 'r': *o++ = '\r'; break;
        case 'b': *o++ = '\b'; break;
        case 'f': *o++ = '\f'; break;
        case 'u': {
            unsigned c1 = hex4(p);
            if (c1 >= 0xD800 && c1 < 0xDC00 && q - p->s >= 6 && p->s[0] == '\\' && p->s[1] == 'u') {
                p->s += 2; c1 = 0x10000 + ((c1 - 0xD800) << 10) + (hex4(p) - 0xDC00);
            }
            o = put8(o, c1); break;
        }
        default: *o++ = c;
        }
    }
    p->s = q + 1;
    return mkCharLenCE(buf, (int) (o - buf), CE_UTF8);
}

static SEXP val(P *p);

static SEXP agg(P *p, int obj) {
    char close = obj ? '}' : ']';
    R_xlen_t n = 0, cap = 4;
    PROTECT_INDEX iv, in;
    SEXP v = allocVector(VECSXP, cap), nm = obj ? allocVector(STRSXP, cap) : R_NilValue;
    PROTECT_WITH_INDEX(v, &iv); PROTECT_WITH_INDEX(nm, &in);
    p->s++; ws(p);
    if (p->s < p->e && *p->s == close) p->s++;
    else for (;;) {
        if (n == cap) {
            cap *= 2;
            REPROTECT(v = xlengthgets(v, cap), iv);
            if (obj) REPROTECT(nm = xlengthgets(nm, cap), in);
        }
        if (obj) { ws(p); if (p->s >= p->e || *p->s != '"') error("invalid JSON"); SET_STRING_ELT(nm, n, str(p)); expect(p, ':'); }
        SET_VECTOR_ELT(v, n++, val(p));
        ws(p);
        if (p->s < p->e && *p->s == ',') { p->s++; continue; }
        expect(p, close); break;
    }
    REPROTECT(v = xlengthgets(v, n), iv);
    if (obj) { REPROTECT(nm = xlengthgets(nm, n), in); setAttrib(v, R_NamesSymbol, nm); }
    UNPROTECT(2);
    return v;
}

static SEXP val(P *p) {
    ws(p);
    if (p->s >= p->e) error("unexpected end of JSON");
    if (*p->s == '{') return agg(p, 1);
    if (*p->s == '[') return agg(p, 0);
    if (*p->s == '"') { SEXP c = PROTECT(str(p)), r = ScalarString(c); UNPROTECT(1); return r; }
    if (!strncmp(p->s, "true", 4)) { p->s += 4; return ScalarLogical(1); }
    if (!strncmp(p->s, "false", 5)) { p->s += 5; return ScalarLogical(0); }
    if (!strncmp(p->s, "null", 4)) { p->s += 4; return R_NilValue; }
    char *end; double d = strtod(p->s, &end);
    if (end == p->s) error("invalid JSON");
    p->s = end;
    return ScalarReal(d);
}

/* Read one framed message from stdin; NULL on EOF. */
static SEXP lsp_read(void) {
    char line[1024];
    long len = -1;
    for (;;) {
        if (!fgets(line, sizeof line, stdin)) return R_NilValue;
        if (!strncmp(line, "Content-Length:", 15)) len = atol(line + 15);
        else if ((line[0] == '\r' || line[0] == '\n') && len >= 0) break;
    }
    char *buf = R_alloc((size_t) len + 1, 1);
    if (fread(buf, 1, (size_t) len, stdin) != (size_t) len) return R_NilValue;
    buf[len] = 0;
    P p = {buf, buf + len};
    return val(&p);
}

/* ---- R -> JSON: named list = object, unnamed list = array, length-1 atomic
 * = scalar (unless class AsIs), other atomics = arrays, NULL/NA = null ---- */
typedef struct { char *b; size_t n, cap; } B;

static void add(B *o, const char *s, size_t k) {
    if (o->n + k > o->cap) {
        char *nb = realloc(o->b, o->cap = 2 * (o->n + k));
        if (!nb) error("out of memory");
        o->b = nb;
    }
    memcpy(o->b + o->n, s, k); o->n += k;
}
static void adds(B *o, const char *s) { add(o, s, strlen(s)); }

static void jstr(B *o, const char *s) {
    char t[8];
    adds(o, "\"");
    for (; *s; s++) {
        unsigned char c = (unsigned char) *s;
        if (c == '"' || c == '\\') { t[0] = '\\'; t[1] = (char) c; add(o, t, 2); }
        else if (c == '\n') adds(o, "\\n");
        else if (c < 0x20) { snprintf(t, sizeof t, "\\u%04x", c); adds(o, t); }
        else add(o, s, 1);
    }
    adds(o, "\"");
}

static void scalar(B *o, SEXP x, R_xlen_t i) {
    char t[32];
    switch (TYPEOF(x)) {
    case STRSXP:
        if (STRING_ELT(x, i) == NA_STRING) adds(o, "null"); else jstr(o, translateCharUTF8(STRING_ELT(x, i)));
        return;
    case LGLSXP: adds(o, LOGICAL(x)[i] == NA_LOGICAL ? "null" : LOGICAL(x)[i] ? "true" : "false"); return;
    case INTSXP:
        if (INTEGER(x)[i] == NA_INTEGER) adds(o, "null"); else { snprintf(t, sizeof t, "%d", INTEGER(x)[i]); adds(o, t); }
        return;
    case REALSXP: {
        double v = REAL(x)[i];
        if (!R_FINITE(v)) adds(o, "null");
        else { snprintf(t, sizeof t, v == floor(v) && fabs(v) < 1e15 ? "%.0f" : "%.15g", v); adds(o, t); }
        return;
    }
    default: adds(o, "null");
    }
}

static void enc(B *o, SEXP x) {
    R_xlen_t n = xlength(x);
    if (isNull(x)) { adds(o, "null"); return; }
    if (TYPEOF(x) == VECSXP) {
        SEXP nm = getAttrib(x, R_NamesSymbol);
        adds(o, isNull(nm) ? "[" : "{");
        for (R_xlen_t i = 0; i < n; i++) {
            if (i) adds(o, ",");
            if (!isNull(nm)) { jstr(o, translateCharUTF8(STRING_ELT(nm, i))); adds(o, ":"); }
            enc(o, VECTOR_ELT(x, i));
        }
        adds(o, isNull(nm) ? "]" : "}");
    } else if (n == 1 && !inherits(x, "AsIs")) scalar(o, x, 0);
    else {
        adds(o, "[");
        for (R_xlen_t i = 0; i < n; i++) { if (i) adds(o, ","); scalar(o, x, i); }
        adds(o, "]");
    }
}

static SEXP lsp_write(SEXP x) {
    static B o = {NULL, 0, 0};
    o.n = 0;
    enc(&o, x);
    fprintf(stdout, "Content-Length: %lu\r\n\r\n", (unsigned long) o.n);
    fwrite(o.b, 1, o.n, stdout);
    fflush(stdout);
    return R_NilValue;
}

/* R parse-data column (1-based, chars, tabs to multiples of 8) -> 0-based
 * UTF-16 offset of that char's start, or of its end when `end` is TRUE. */
static SEXP cols16(SEXP lines, SEXP ln, SEXP col, SEXP end) {
    R_xlen_t n = XLENGTH(col);
    int e = asLogical(end);
    SEXP r = PROTECT(allocVector(INTSXP, n));
    for (R_xlen_t i = 0; i < n; i++) {
        int L = INTEGER(ln)[i], c = INTEGER(col)[i], cn = 0, u = 0;
        const char *s = (L >= 1 && L <= XLENGTH(lines)) ? translateCharUTF8(STRING_ELT(lines, L - 1)) : "";
        while (*s) {
            int w = ((unsigned char) *s >= 0xF0) ? 2 : 1;
            cn = (*s == '\t') ? ((cn + 8) & ~7) : cn + 1;
            if (cn >= c) { if (e) u += w; break; }
            u += w;
            for (s++; (*s & 0xC0) == 0x80; s++);
        }
        INTEGER(r)[i] = u;
    }
    UNPROTECT(1);
    return r;
}

static const R_CallMethodDef calls[] = {
    {"lsp_read", (DL_FUNC) &lsp_read, 0},
    {"lsp_write", (DL_FUNC) &lsp_write, 1},
    {"cols16", (DL_FUNC) &cols16, 4},
    {NULL, NULL, 0}
};

void R_init_languageserver(DllInfo *dll) {
#ifdef _WIN32
    _setmode(_fileno(stdin), _O_BINARY);
    _setmode(_fileno(stdout), _O_BINARY);
#endif
    R_registerRoutines(dll, NULL, calls, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
