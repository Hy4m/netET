#' @title Circular Layout With FR
#' @description arranges nodes in circles and each group center is determine by
#' Fruchterman-Reingold layout.
#' @param g igraph object.
#' @param group if NULL (default), all nodes will be treated as single group.
#' @param ... other parameters passing to \code{layout_in_circular()} function.
#' @return a two-columns matrix.
#' @family layout
#' @rdname layout_fr_circle
#' @author Hou Yun
#' @export
layout_fr_circle <- function(g, group = NULL, ...) {
  if (empty_graph(g)) {
    return(matrix(nrow = 0, ncol = 2))
  }
  group <- rlang::enquo(group)
  if (rlang::quo_is_null(group)) {
    return(igraph::layout_with_fr(g, ...))
  }

  nodes <- igraph::as_data_frame(g, "vertices")
  group <- rlang::eval_tidy(group, nodes)
  group <- rep_len(group, nrow(nodes))
  adj <- make_weights(g, group)
  g2 <- igraph::graph_from_adjacency_matrix(adj,
                                            mode = "undirected",
                                            weighted = TRUE)
  coord <- igraph::layout_with_fr(g2)
  center <- list(x = stats::setNames(coord[, 1, drop = TRUE], rownames(adj)),
                 y = stats::setNames(coord[, 2, drop = TRUE], rownames(adj)))

  layout_in_circle(g = g,
                   group = group,
                   center = center,
                   ...)
}

#' @noRd
make_weights <- function(g, group) {
  nodes <- igraph::vertex_attr(g, "name")
  group <- split(nodes, group)
  edges <- igraph::as_data_frame(g, "edges")
  nm <- names(group)

  n <- matrix(0,
              nrow = length(group),
              ncol = length(group),
              dimnames = list(nm, nm))
  for (ii in seq_along(nm)) {
    for (jj in seq_along(nm)) {
      if (ii <= jj) next
      v <- group[[ii]]
      v2 <- group[[jj]]
      id <- ((edges$from %in% v) & (edges$to %in% v2)) |
        ((edges$from %in% v2) & (edges$to %in% v))
      n[ii, jj] <- sum(id)
    }
  }
  print(n)
  n
}
