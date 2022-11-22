#' @title Network Attributions
#' @description helper functions to get nodes/edges/graph attributions.
#' @param g a igraph object.
#' @param module if NULL (default), will uses \code{igraph::cluster_fast_greedy()}
#' function to calculate module index.
#' @param degree if NULL (default), will uses \code{igraph::degree()} to calculate
#' degree attributes of nodes.
#' @param ... other parameters passing to \code{igraph::cluster_fast_greedy()}.
#' @return a numeric vectors.
#' @rdname attributes
#' @author Hou Yun
#' @export
z_score <- function(g, module = NULL, ...) {
  module <- rlang::enquo(module)
  if (rlang::quo_is_null(module)) {
    memb <- igraph::membership(igraph::cluster_fast_greedy(g, ...))
  } else {
    nodes <- igraph::as_data_frame(g, "vertices")
    memb <- rlang::eval_tidy(module, nodes)
    memb <- rep_len(memb, nrow(memb))
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
                    module = NULL,
                    degree = NULL,
                    ...) {
  module <- rlang::enquo(module)
  degree <- rlang::enquo(degree)
  nodes <- igraph::as_data_frame(g, "vertices")
  if (rlang::quo_is_null(module)) {
    memb <- igraph::membership(igraph::cluster_fast_greedy(g, ...))
  } else {
    memb <- rlang::eval_tidy(module, nodes)
    memb <- rep_len(memb, nrow(memb))
  }

  if (rlang::quo_is_null(degree)) {
    degree <- igraph::degree(g)
  } else {
    degree <- rlang::eval_tidy(degree, nodes)
    degree <- rep_len(degree, nrow(nodes))
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
centrality_in_module <- function(module = NULL, ...) {
  expect_nodes()
  graph <- as.igraph(tidygraph::.G())
  z_score(graph, module = module, ...)
}

#' @rdname attributes
#' @export
centrality_between_module <- function(module = NULL,
                                      degree = NULL,
                                      ...) {
  expect_nodes()
  graph <- as.igraph(tidygraph::.G())
  p_score(graph, module = module, degree = degree, ...)
}
