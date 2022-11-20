#' @title Circular Layout With FR
#' @description arranges nodes in circles and each group center is determine by
#' Fruchterman-Reingold layout.
#' @param g igraph object.
#' @param group_vars if NULL (default), all nodes will be treated as single group.
#' @param ... other parameters passing to \code{layout_in_circular()} function.
#' @return a two-columns matrix.
#' @family layout
#' @rdname layout_fr_circular
#' @author Hou Yun
#' @export
layout_fr_circular <- function(g, group_vars = NULL, ...) {
  if (empty_graph(g)) {
    return(matrix(nrow = 0, ncol = 2))
  }

  if (is.null(group_vars)) {
    center <- list(x = 0, y = 0)
  } else {
    adj <- make_weights(g, igraph::vertex_attr(g, group_vars))
    g2 <- igraph::graph_from_adjacency_matrix(adj,
                                              mode = "undirected",
                                              weighted = TRUE)
    coord <- igraph::layout_with_fr(g2)
    center <- list(x = stats::setNames(coord[, 1, drop = TRUE], rownames(adj)),
                   y = stats::setNames(coord[, 2, drop = TRUE], rownames(adj)))
  }
  layout_in_circular(g = g,
                     group_vars = group_vars,
                     center = center,
                     ...)
}

#' @noRd
make_weights <- function(g, group) {
  nodes <- igraph::vertex_attr(g, "name")
  group <- split(nodes, group)
  edges <- igraph::as_data_frame(g, "edges")

  n <- matrix(0,
              nrow = length(group),
              ncol = length(group),
              dimnames = list(names(group), names(group)))
  for (ii in names(group)) {
    for (jj in names(group)) {
      if (ii == jj) next
      v <- group[[ii]]
      v2 <- group[[jj]]
      id <- ((edges$from %in% v) & (edges$to %in% v2)) ||
        ((edges$from %in% v2) & (edges$to %in% v))
      n[ii, jj] <- sum(id)
    }
  }
  n
}
