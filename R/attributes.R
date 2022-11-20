#' @title Network Attributions
#' @description helper functions to get nodes/edges/graph attributions.
#' @param g a igraph object.
#' @param module_vars if NULL (default), will uses \code{igraph::cluster_fast_greedy()}
#' function to calculate module index.
#' @param degree_vars if NULL (default), will uses \code{igraph::degree()} to calculate
#' degree attributes of nodes.
#' @param ... other parameters passing to \code{igraph::cluster_fast_greedy()}.
#' @return a numeric vectors.
#' @rdname attributes
#' @author Hou Yun
#' @export
z_score <- function(g, module_vars = NULL, ...) {
  if (is.null(module_vars)) {
    memb <- igraph::membership(igraph::cluster_fast_greedy(g, ...))
  } else {
    memb <- igraph::vertex_attr(g, module_vars)
  }

  adj <- igraph::as_adj(g, sparse = FALSE, names = TRUE)

  ## copy from brainGraph package
  N <- max(memb)
  nS <- tabulate(memb)
  z <- ki <- rep(0L, nrow(adj))
  Ksi <- sigKsi <- rep(0L, N)

  for (ii in seq_len(N)) {
    x <- adj[memb == ii, ] %*% (memb == ii)
    ki[memb == ii] <- x
    Ksi[ii] <- mean(x)
    sigKsi[ii] <- stats::sd(x)
  }

  z <- (ki - Ksi[memb]) / sigKsi[memb]
  z[is.infinite(z) | is.nan(z)] <- NA
  z
}

#' @rdname attributes
#' @export
p_score <- function(g,
                    module_vars = NULL,
                    degree_vars = NULL,
                    ...) {
  if (is.null(module_vars)) {
    memb <- igraph::membership(igraph::cluster_fast_greedy(g, ...))
  } else {
    memb <- igraph::vertex_attr(g, module_vars)
  }
  if (is.null(degree_vars)) {
    degree <- igraph::degree(g)
  } else {
    degree <- igraph::vertex_attr(g, degree_vars)
  }
  adj <- igraph::as_adj(g, sparse = FALSE, names = TRUE)

  ## copy from brainGraph package
  N <- max(memb)
  Kis <- vapply(seq_len(N), function(x) colSums(adj * (memb == x)),
                numeric(length(degree)))

  pi <- 1 - ((1/degree^2) * rowSums(Kis^2))

  pi[is.infinite(pi) | is.nan(pi)] <- NA
  pi
}

#' @rdname attributes
#' @export
centrality_in_module <- function(module_vars = NULL, ...) {
  expect_nodes()
  graph <- as.igraph(tidygraph::.G())
  z_score(graph, module_vars = module_vars, ...)
}

#' @rdname attributes
#' @export
centrality_between_module <- function(module_vars = NULL,
                                      degree_vars = NULL,
                                      ...) {
  expect_nodes()
  graph <- as.igraph(tidygraph::.G())
  p_score(graph, module_vars = module_vars, degree_vars = degree_vars, ...)
}
