#' @title Liner layout
#' @description this layout puts all nodes on a line.
#' @param g a igraph object.
#' @param ... condition of sort by, see `?dplyr::arrange` for details.
#' @param angle slope of line (in degree).
#' @return a two-columns matrix of position.
#' @author Hou Yun
#' @rdname layout_nice_liner
#' @export
layout_nice_liner <- function(g, ..., angle = 0) {
  if (empty_graph(g)) {
    return(matrix(nrow = 0, ncol = 2))
  }

  nodes <- igraph::as_data_frame(g, "vertices")
  new_nodes <- dplyr::arrange(nodes, ...)
  ids <- ids(new_nodes$name, nodes$name)

  n <- nrow(nodes)
  angle <- angle %% 180
  if (identical(angle, 90)) {
    x <- rep_len(0, n)
    y <- seq(-1, 1, length.out = n)
  } else {
    angle <- angle * pi / 180
    x <- seq(-1, 1, length.out = n)
    y <- tan(angle) * x
  }
  xy <- matrix(c(x, y), ncol = 2, byrow = FALSE)
  reorder_by_ids(xy, ids)
}
