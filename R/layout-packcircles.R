#' @title Packcircle Layout
#' @description arranges nodes by degree.
#' @param g igraph object.
#' @param degree if NULL (default), will use \code{igraph::degree()} to
#' calculate degree.
#' @param ... other parameters passing to
#' \code{packcircles::circleProgressiveLayout()} function.
#' @return a two-columns matrix.
#' @family layout
#' @rdname layout_packcircles
#' @author Hou Yun
#' @export
layout_packcircles <- function(g, degree = NULL, ...) {
  if (empty_graph(g)) {
    return(matrix(nrow = 0, ncol = 2))
  }

  degree <- rlang::enquo(degree)
  if (rlang::quo_is_null(degree)) {
    degree <- igraph::degree(g)
  } else {
    nodes <- igraph::as_data_frame(g, "vertices")
    degree <- rlang::eval_tidy(degree, nodes)
    degree <- rep_len(degree, nrow(nodes))
  }
  circleProgressiveLayout <- get_function("packcircles", "circleProgressiveLayout")
  xy <- circleProgressiveLayout(x = degree, ...)
  as.matrix(xy[1:2])
}
