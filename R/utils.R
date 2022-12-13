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
    nrow(df) == 0 || ncol(df) == 0
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

#' @noRd
zoomin_circle <- function(mat, cx, cy, r) {
  x <- mat[, 1, drop = TRUE]
  y <- mat[, 2, drop = TRUE]
  cx0 <- mean(x, na.rm = TRUE)
  cy0 <- mean(y, na.rm = TRUE)
  x <- x - cx0
  y <- y - cy0

  d <- euclidean_dist(x, y, 0, 0)
  z <- r / max(d, na.rm = TRUE)
  x <- x*z + cx
  y <- y*z + cy
  matrix(c(x, y), ncol = 2, byrow = FALSE)
}

#' @noRd
ids <- function(x, y) {
  vapply(x, function(.x) {
    which(.x == y)
  }, numeric(1), USE.NAMES = FALSE)
}

#' @noRd
reorder_by_ids <- function(mat, ids) {
  ids <- stats::setNames(ids, seq_along(ids))
  ids <- as.integer(names(sort(ids)))
  mat[ids, , drop = FALSE]
}

#' @noRd
quo_is_character <- function(x) {
  is.character(rlang::quo_get_expr(x))
}

#' @noRd
set_identical <- function(x, y) {
  all(x %in% y) && all(y %in% x)
}

#' @noRd
rename <- function (data, ...)
{
  ll <- list(...)
  if (length(ll) == 0) {
    data
  } else {
    nm <- names(data)
    old <- unname(unlist(ll))
    new <- names(ll)

    old <- old[old %in% nm]
    new <- new[old %in% nm]
    names(data)[names(data) %in% old] <- new
  }
  data
}

#' @noRd
calc_degree <- function(start, end) {
  n <- max(length(start), length(end))
  start <- rep_len(start %% 360, n)
  end <- rep_len(end %% 360, n)

  end <- ifelse(start == end | end == 0, end + 360, end)
  temp <- start
  start <- ifelse(start > end, end, start)
  end <- ifelse(temp > end, temp, end)

  list(start = start, end = end)
}
