`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

#' @noRd
get_function <- function (pkg, fun)
{
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(pkg, " package has not been installed", call. = FALSE)
  }
  eval(parse(text = paste0(pkg, "::", fun)))
}

#' @noRd
empty <- function (df)
{
  if (inherits(df, "data.frame") || inherits(df, "matrix")) {
    is.null(df) || nrow(df) == 0 || ncol(df) == 0
  } else {
    is.null(df) || length(df) == 0
  }
}

#' @noRd
empty_graph <- function(g) {
  if (!igraph::is.igraph(g)) {
    g <- igraph::as.igraph(g)
  }
  igraph::vcount(g) < 1
}

#' @noRd
#' @importFrom utils modifyList
aes_modify <- function (aes1, aes2)
{
  aes <- utils::modifyList(as.list(aes1), as.list(aes2))
  class(aes) <- "uneval"
  aes
}

#' @noRd
expect_nodes <- getFromNamespace("expect_nodes", "tidygraph")

#' @noRd
expect_edges <- getFromNamespace("expect_edges", "tidygraph")
