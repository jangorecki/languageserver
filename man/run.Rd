\name{run}
\alias{run}
\title{Run the R language server}
\usage{
run(debug = FALSE, ...)
}
\arguments{
\item{debug}{log incoming methods and errors to stderr}
\item{...}{ignored (kept for compatibility with the original signature)}
}
\description{
Speaks the Language Server Protocol (JSON-RPC) on stdin/stdout until the
client closes the stream or sends \code{exit}.
}
