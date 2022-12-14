#' @title Normalize matrix
#' @description Normalize layout matrix in unit circle.
#' @param mat a layout matrix.
#' @return a normalized matrix.
#' @rdname normalize
#' @author Hou Yun
#' @export
layout_normalize <- function(mat) {
  if (empty(mat)) {
    return(mat)
  }

  x <- mat[, 1, drop = TRUE]
  y <- mat[, 2, drop = TRUE]
  rng_x <- range(x, na.rm = TRUE)
  rng_y <- range(y, na.rm = TRUE)

  if (any(is.infinite(rng_x), is.infinite(rng_y))) {
    warning("Contain infinite value.", call. = FALSE)
    return(mat)
  }

  if (identical(diff(rng_x), 0)) {
    y <- scales::rescale(y, c(-1, 1), rng_y)
  } else if (identical(diff(rng_y), 0)) {
    x <- scales::rescale(x, c(-1, 1), rng_x)
  } else {
    ratio <- diff(rng_x)/diff(rng_y)
    if (ratio > 1) {
      x <- scales::rescale(x, c(-1, 1), rng_x)
      y <- scales::rescale(y, c(-1, 1)/ratio, rng_y)
    } else {
      x <- scales::rescale(x, c(-1, 1)*ratio, rng_x)
      y <- scales::rescale(y, c(-1, 1), rng_y)
    }
  }
  mat[, 1] <- x
  mat[, 2] <- y
  mat
}
