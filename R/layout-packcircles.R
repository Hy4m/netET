#' @title Packcircle Layout
#' @description arranges nodes by degree.
#' @param g igraph object.
#' @param degree_vars if NULL (default), will use \code{igraph::degree()} to
#' calculate degree.
#' @param ... other parameters passing to
#' \code{packcircles::circleProgressiveLayout()} function.
#' @return a two-columns matrix.
#' @family layout
#' @rdname layout_packcircles
#' @author Hou Yun
#' @export
layout_packcircles <- function(g, degree_vars = NULL, ...) {
  if (empty_graph(g)) {
    return(matrix(nrow = 0, ncol = 2))
  }

  if (is.null(degree_vars)) {
    degree <- igraph::degree(g)
  } else {
    degree <- igraph::vertex_attr(g, degree_vars)
  }
  circleProgressiveLayout <- get_function("packcircles", "circleProgressiveLayout")
  xy <- circleProgressiveLayout(x = degree, ...)
  as.matrix(xy[1:2])
}
