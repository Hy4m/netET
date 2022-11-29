#' @title Circular Layout
#' @description arranges nodes in circles according to group information.
#' @param g igraph object.
#' @param layout function used to calculate node positions.
#' @param center a list named with x and y.
#' @param group if NULL (default), all nodes will be treated as single group.
#' @param zoom numeric, range in [0, 1].
#' @param by_nodes logical, indicating whether to scale based on the number of nodes.
#' @param ... other parameters passing to \code{layout} function.
#' @return a two-columns matrix.
#' @family layout
#' @rdname layout_circle
#' @author Hou Yun
#' @export
layout_in_circle <- function(g,
                             layout = NULL,
                             group = NULL,
                             center = NULL,
                             zoom = 0.9,
                             by_nodes = FALSE,
                             ...) {
  if (empty_graph(g)) {
    return(matrix(nrow = 0, ncol = 2))
  }

  group <- rlang::enquo(group)
  if (rlang::quo_is_null(group)) {
    group <- rep_len(1L, igraph::vcount(g))
  } else {
    nodes <- igraph::as_data_frame(g, "vertices")
    group <- rlang::eval_tidy(group, nodes)
    group <- rep_len(group, nrow(nodes))
  }

  layout <- layout %||% igraph::layout.auto
  gen_in_circle(g, layout, as.character(group), center, zoom, by_nodes, ...)
}

#' @rdname layout_circle
#' @family layout
#' @export
layout_on_circle <- function(g,
                             group = NULL,
                             center = NULL,
                             zoom = 0.618,
                             by_nodes = TRUE) {
  if (empty_graph(g)) {
    return(matrix(nrow = 0, ncol = 2))
  }

  group <- rlang::enquo(group)
  if (rlang::quo_is_null(group)) {
    group <- rep_len(1L, igraph::vcount(g))
  } else {
    nodes <- igraph::as_data_frame(g, "vertices")
    group <- rlang::eval_tidy(group, nodes)
    group <- rep_len(group, nrow(nodes))
  }

  nodes <- igraph::vertex_attr(g, "name")
  gen_circle(nodes, as.character(group), center, zoom, by_nodes)
}

#' @rdname layout_circle
#' @family layout
#' @export
layout_zoomin_circle <- function(g,
                                 layout = NULL,
                                 center = NULL,
                                 ...) {
  layout <- layout %||% igraph::layout.auto
  xy <- layout(g, ...)
  if (nrow(xy) < 1) {
    return(matrix(ncol = 2, nrow = 0))
  }

  x <- xy[, 1, drop = TRUE]
  y <- xy[, 2, drop = TRUE]
  if (is.null(center)) {
    cx <- mean(x, na.rm = TRUE)
    cy <- mean(y, na.rm = TRUE)
  } else {
    cx <- center$x %||% mean(x, na.rm = TRUE)
    cy <- center$y %||% mean(y, na.rm = TRUE)
  }

  zoomin_circle(xy, cx, cy, 1)
}

#' @noRd
gen_circle <- function(nodes, group, center, zoom = 0.618, by_nodes = TRUE) {
  group <- split(nodes, group)
  ll <- length(group)
  nm <- names(group)
  n <- vapply(group, length, numeric(1))

  if (is.null(center)) {
    if (ll == 1L) {
      cx <- stats::setNames(0, nm)
      cy <- stats::setNames(0, nm)
    } else {
      tt <- seq(0, 2 * pi, length.out = ll + 1L)[-(ll + 1L)]
      cx <- stats::setNames(cos(tt), nm)
      cy <- stats::setNames(sin(tt), nm)
    }
  } else {
    cx <- get_center(center$x, nm)
    cy <- get_center(center$y, nm)
  }

  if(ll == 1L) {
    if (n == 1) {
      xy <- matrix(c(cx, cy), ncol = 2, byrow = FALSE)
    } else {
      t <- seq(0, 2 * pi, length.out = n + 1L)[-(n + 1L)]
      xy <- matrix(c(cos(t) + cx, sin(t) + cy), ncol = 2, byrow = FALSE)
    }
  } else {
    r <- min(
      vapply(nm, function(.nm) {
        other <- setdiff(nm, .nm)
        min(euclidean_dist(cx[other], cy[other], cx[.nm], cy[.nm]), na.rm = TRUE)/2
      }, numeric(1)),
      na.rm = TRUE
    )

    if (isTRUE(by_nodes)) {
      r <- stats::setNames(sqrt(abs(zoom)) * r * sqrt(n/max(n)), nm)
    } else {
      r <- stats::setNames(rep_len(sqrt(abs(zoom)) * r, ll), nm)
    }

    ids <- unlist(unname(lapply(group, function(g) ids(g, nodes))))
    xy <- matrix(nrow = 0, ncol = 2)
    for (ii in nm) {
      if (n[ii] == 1) {
        xy <- rbind(xy, matrix(c(cx[ii], cy[ii]),
                               ncol = 2,
                               byrow = FALSE))
      } else {
        t <- seq(0, 2 * pi, length.out = n[ii] + 1L)[-(n[ii] + 1L)]
        xy <- rbind(xy, matrix(c(r[ii] * cos(t) + cx[ii], r[ii] * sin(t) + cy[ii]),
                               ncol = 2,
                               byrow = FALSE))
      }
    }
    xy <- reorder_by_ids(xy, ids)
  }
  xy
}

#' @noRd
gen_in_circle <- function(g, layout, group, center, zoom = 0.9, by_nodes = TRUE, ...) {
  nodes <- igraph::vertex_attr(g, "name")
  group <- split(nodes, group)
  n <- vapply(group, length, numeric(1))
  ll <- length(group)
  nm <- names(group)

  if (is.null(center)) {
    if (ll == 1L) {
      cx <- stats::setNames(0, nm)
      cy <- stats::setNames(0, nm)
    } else {
      tt <- seq(0, 2 * pi, length.out = ll + 1L)[-(ll + 1L)]
      cx <- stats::setNames(cos(tt), nm)
      cy <- stats::setNames(sin(tt), nm)
    }
  } else {
    cx <- get_center(center$x, nm)
    cy <- get_center(center$y, nm)
  }

  coords <- structure(as.list(rep_len(NA, ll)), names = nm)
  for (ii in nm) {
    subg <- igraph::subgraph(g, which(nodes %in% group[[ii]]))
    coords[[ii]] <- layout(subg, ...)
  }

  if(ll == 1L) {
    xy <- zoomin_circle(coords[[nm]], cx, cy, 1)
  } else {
    r <- min(
      vapply(nm, function(.nm) {
        other <- setdiff(nm, .nm)
        min(euclidean_dist(cx[other], cy[other], cx[.nm], cy[.nm]), na.rm = TRUE)/2
      }, numeric(1)),
      na.rm = TRUE
    )

    if (isTRUE(by_nodes)) {
      r <- stats::setNames(sqrt(abs(zoom)) * r * sqrt(n/max(n)), nm)
    } else {
      r <- stats::setNames(rep_len(sqrt(abs(zoom)) * r, ll), nm)
    }

    ids <- unlist(lapply(group, function(g) ids(g, nodes)))
    xy <- matrix(nrow = 0, ncol = 2)
    for (ii in nm) {
      xy <- rbind(xy, zoomin_circle(coords[[ii]], cx[ii], cy[ii], r[ii]))
    }
    xy <- reorder_by_ids(xy, ids)
  }
  xy
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
