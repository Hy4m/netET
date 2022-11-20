#' @title Circular Layout
#' @description arranges nodes in circles according to group information.
#' @param g igraph object.
#' @param layout function used to calculate node positions.
#' @param center a list named with x and y.
#' @param group_vars if NULL (default), all nodes will be treated as single group.
#' @param zoom numeric, range in [0, 1].
#' @param ... other parameters passing to \code{layout} function.
#' @return a two-columns matrix.
#' @family layout
#' @rdname layout_circular
#' @author Hou Yun
#' @export
layout_in_circular <- function(g,
                               layout = NULL,
                               group_vars = NULL,
                               center = NULL,
                               zoom = 0.618,
                               ...) {
  if (empty_graph(g)) {
    return(matrix(nrow = 0, ncol = 2))
  }
  if (is.null(group_vars)) {
    group <- rep_len(1L, igraph::vcount(g))
  } else {
    group <- igraph::vertex_attr(g, group_vars)
  }

  layout <- layout %||% igraph::layout.auto
  xy <- gen_in_circle(g, layout, as.character(group), center, zoom, ...)
  as.matrix(xy[1:2])
}

#' @rdname layout_circular
#' @family layout
#' @export
layout_on_circular <- function(g,
                               group_vars = NULL,
                               center = NULL,
                               zoom = 0.618) {
  if (empty_graph(g)) {
    return(matrix(nrow = 0, ncol = 2))
  }

  if (is.null(group_vars)) {
    group <- rep_len(1L, igraph::vcount(g))
  } else {
    group <- igraph::vertex_attr(g, group_vars)
  }

  nodes <- igraph::vertex_attr(g, "name")

  as.matrix(gen_circle(nodes, as.character(group), center, zoom)[1:2])
}

#' @rdname layout_circular
#' @family layout
#' @export
layout_zoomin_circle <- function(g, layout = NULL, zoom = 0.97, ...) {
  layout <- layout %||% igraph::layout.auto
  xy <- layout(g, ...)
  if (nrow(xy) < 1) {
    return(matrix(ncol = 2, nrow = 0))
  }

  x <- xy[, 1, drop = TRUE]
  y <- xy[, 2, drop = TRUE]
  cx <- sum(range(x, na.rm = TRUE))/2
  cy <- sum(range(y, na.rm = TRUE))/2
  d <- euclidean_dist(x, y, cx, cy)
  r <- max(d, na.rm = TRUE)/sqrt(abs(zoom))

  id <- abs(r - d)/2 < 10^(-5)
  delta <- d/(r - d)
  xx <- (cx + delta*x)/(1 + delta)
  yy <- (cy + delta*y)/(1 + delta)
  xx <- ifelse(id, x, xx)
  yy <- ifelse(id, y, yy)

  matrix(c(xx, yy), ncol = 2, byrow = FALSE)
}

#' @noRd
gen_circle <- function(nodes, group, center, zoom = 0.618) {
  group <- split(nodes, group)
  ll <- length(group)
  nm <- names(group)
  n <- vapply(group, length, numeric(1))

  if(ll == 1L) {
    if (is.null(center)) {
      cx <- 0
      cy <- 0
    } else {
      cx <- center$x[1]
      cy <- center$y[1]
    }
    if (n == 1) {
      xy <- tibble::tibble(x = cx,
                           y = cy,
                           nodes = nodes)
    } else {
      t <- seq(0, 2 * pi, length.out = n + 1L)[-(n + 1L)]
      xy <- tibble::tibble(x = cos(t) + cx,
                           y = sin(t) + cy,
                           nodes = nodes)
    }
  } else {
    if (!is.null(center)) {
      cx <- center$x
      cy <- center$y
      cx <- get_center(cx, nm)
      cy <- get_center(cy, nm)
    } else {
      t1 <- seq(0, 2 * pi, length.out = ll + 1L)[-(ll + 1L)]
      cx <- stats::setNames(cos(t1), nm)
      cy <- stats::setNames(sin(t1), nm)
    }

    min_dist <- vapply(nm, function(.nm) {
      other <- setdiff(nm, .nm)
      min(euclidean_dist(cx[other], cy[other], cx[.nm], cy[.nm]), na.rm = TRUE)
    }, numeric(1))
    r <- stats::setNames(sqrt(abs(zoom)) * min(min_dist)/2 * sqrt(n) / max(sqrt(n)), nm)

    xy <-  tibble::tibble(x = numeric(0),
                          y = numeric(0),
                          nodes = character(0))
    for (ii in nm) {
      if (n[ii] == 1) {
        xy <- dplyr::bind_rows(xy, tibble::tibble(x = cx[ii],
                                                  y = cy[ii],
                                                  nodes = group[[ii]]))
      } else {
        t <- seq(0, 2 * pi, length.out = n[ii] + 1L)[-(n[ii] + 1L)]
        xy <- dplyr::bind_rows(xy, tibble::tibble(x = r[ii] * cos(t) + cx[ii],
                                                  y = r[ii] * sin(t) + cy[ii],
                                                  nodes = group[[ii]]))
      }
    }
  }
  xy[linkET::get_order(nodes, xy$nodes), ]
}

gen_in_circle <- function(g, layout, group, center, zoom = 0.618, ...) {
  nodes <- igraph::vertex_attr(g, "name")
  group <- split(nodes, group)
  ll <- length(group)
  nm <- names(group)
  n <- vapply(group, length, numeric(1))

  coords <- structure(as.list(rep_len(NA, ll)), names = nm)
  for (ii in nm) {
    subg <- igraph::subgraph(g, group[[ii]])
    coords[[ii]] <- layout(subg, ...)
  }

  if(ll == 1L) {
    if (is.null(center)) {
      cx <- 0
      cy <- 0
    } else {
      cx <- get_center(center$x, nm)
      cy <- get_center(center$y, nm)
    }
    if (n == 1) {
      xy <- tibble::tibble(x = cx,
                           y = cy,
                           nodes = nodes)
    } else {
      x <- coords[[ii]][, 1, drop = TRUE]
      y <- coords[[ii]][, 2, drop = TRUE]
      diff_x <- cx - sum(range(x, na.rm = TRUE))/2
      diff_y <- cy - sum(range(y, na.rm = TRUE))/2
      x <- x + diff_x
      y <- y + diff_y
      d <- euclidean_dist(x, y, cx, cy)
      r <- max(d, na.rm = TRUE)/sqrt(0.97)

      id <- abs(r - d)/2 < 10^(-5)
      delta <- d/(r - d)
      xx <- (cx + delta*x)/(1 + delta)
      yy <- (cy + delta*y)/(1 + delta)
      xx <- ifelse(id, x, xx)
      yy <- ifelse(id, y, yy)
      xy <- tibble::tibble(x = xx,
                           y = yy,
                           nodes = nodes)
    }
  } else {
    if (!is.null(center)) {
      cx <- center$x
      cy <- center$y
      cx <- get_center(cx, nm)
      cy <- get_center(cy, nm)
    } else {
      t1 <- seq(0, 2 * pi, length.out = ll + 1L)[-(ll + 1L)]
      cx <- stats::setNames(cos(t1), nm)
      cy <- stats::setNames(sin(t1), nm)
    }

    min_dist <- vapply(nm, function(.nm) {
      other <- setdiff(nm, .nm)
      min(euclidean_dist(cx[other], cy[other], cx[.nm], cy[.nm]), na.rm = TRUE)
    }, numeric(1))
    r <- stats::setNames(sqrt(abs(zoom)) * min(min_dist)/2 * sqrt(n) / max(sqrt(n)), nm)

    xy <-  tibble::tibble(x = numeric(0),
                          y = numeric(0),
                          nodes = character(0))
    for (ii in nm) {
      if (n[ii] == 1) {
        xy <- dplyr::bind_rows(xy, tibble::tibble(x = cx[ii],
                                                  y = cy[ii],
                                                  nodes = group[[ii]]))
      } else {
        x <- coords[[ii]][, 1, drop = TRUE]
        y <- coords[[ii]][, 2, drop = TRUE]
        diff_x <- cx[ii] - sum(range(x, na.rm = TRUE))/2
        diff_y <- cy[ii] - sum(range(y, na.rm = TRUE))/2
        x <- x + diff_x
        y <- y + diff_y
        d <- euclidean_dist(x, y, cx, cy)
        r <- max(d, na.rm = TRUE)/sqrt(0.97)

        id <- abs(r - d)/2 < 10^(-5)
        delta <- d/(r - d)
        xx <- (cx[ii] + delta*x)/(1 + delta)
        yy <- (cy[ii] + delta*y)/(1 + delta)
        xx <- ifelse(id, x, xx)
        yy <- ifelse(id, y, yy)
        xy <- dplyr::bind_rows(xy, tibble::tibble(x = xx,
                                                  y = yy,
                                                  nodes = group[[ii]]))
      }
    }
  }
  xy[linkET::get_order(nodes, xy$nodes), ]
}

#' @noRd
semi_diameter <- function(x, y) {
  n <- max(length(x), length(y))
  x <- rep_len(x, n)
  y <- rep_len(y, n)
  if (n == 1) {
    out <- 1
  } else {
    out <- vector("numeric", n)
    for (ii in seq_len(n)) {
      out[ii] <- max(sqrt((x[-ii] - x[ii])^2 + (y[-ii] - y[ii])^2)/2)
    }
  }
  out
}

#' @noRd
euclidean_dist <- function(x, y, cx, cy) {
  sqrt((x - cx)^2 + (y - cy)^2)
}

#' @noRd
get_center <- function(x, name) {
  nm <- names(x)
  if (is.null(nm)) {
    out <- stats::setNames(rep_len(x, length(name)), name)
  } else {
    inner <- intersect(nm, name)
    other <- nm[nm == ""]
    if ((length(inner) + length(other)) < length(name)) {
      stop("'center' parameter is so short.", call. = FALSE)
    }
    out <- vector("numeric", length(name))
    names(out) <- name
    out[name %in% nm] <- x[name[name %in% nm]]
    out[!name %in% nm] <- x[nm == ""][seq_len(length(name) - length(inner))]
  }
  out
}
