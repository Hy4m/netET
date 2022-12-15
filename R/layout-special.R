#' @title Special layout
#' @description Special layout of arranging node on a half-circle.
#' @param g a igraph object created by `as_bipartite_circular()`.
#' @param ... not used.
#' @return a layout matrix.
#' @author Hou Yun
#' @rdname layout_bipartite_circular
#' @export
layout_bipartite_circular <- function(g, ...) {
  if (!inherits(g, "bipartite_circular_graph")) {
    stop("Need a bipartite_circular_graph object,\n",
         "which can be created by as_bipartite_circular().", call. = FALSE)
  }

  if (empty_graph(g)) {
    return(matrix(nrow = 0, ncol = 2))
  }

  nodes <- igraph::as_data_frame(g, "vertices")
  x <- ifelse(nodes$.isInner,
              nodes$.r0 * cos(nodes$.angle),
              nodes$.r1 * cos(nodes$.angle))
  y <- ifelse(nodes$.isInner,
              nodes$.r0 * sin(nodes$.angle),
              nodes$.r1 * sin(nodes$.angle))

  matrix(c(nodes$.r * cos(nodes$.angle), nodes$.r * sin(nodes$.angle)),
         ncol = 2, byrow = FALSE)
}

#' @title Special layout
#' @description Special layout of arranging node on a half-circle.
#' @param g a igraph object.
#' @param inner_nodes,outer_nodes character vector indicating which nodes are
#' internal and which are external.
#' @param start start angle in degree.
#' @param end end angle in degree.
#' @param r0 inner radius of arc-rect.
#' @param r1 outer radius of arc-rect.
#' @param ... other parameters passing to `igraph::as.igraph()`.
#' @return a igraph object.
#' @author Hou Yun
#' @rdname as_bipartite_circular
#' @export
as_bipartite_circular <- function(g,
                                  inner_nodes = NULL,
                                  outer_nodes = NULL,
                                  start = 0,
                                  end = 180,
                                  r0 = 0.6,
                                  r1 = 1,
                                  ...) {
  if (!igraph::is.igraph(g)) {
    g <- as.igraph(g, ...)
  }
  if (empty_graph(g)) {
    return(structure(g, class = c("bipartite_circular_graph", class(g))))
  }

  nodes <- igraph::vertex_attr(g, "name")
  edges <- igraph::as_data_frame(g, "edges")

  if (is.null(inner_nodes) && is.null(outer_nodes)) {
    if (empty(edges) || length(intersect(edges$from, edges$to)) > 0) {
      stop("Please set `inner_nodes` or `outer_nodes` parameter.", call. = FALSE)
    }
    inner_nodes <- if (length(unique(edges$from)) > length(unique(edges$to))) {
      unique(edges$to)
    } else {
      unique(edges$from)
    }
    outer_nodes <- setdiff(nodes, inner_nodes)
  } else if (!any(is.null(inner_nodes), is.null(outer_nodes))) {
    inner_nodes <- unique(inner_nodes)
    outer_nodes <- unique(outer_nodes)
  } else {
    if (!is.null(inner_nodes)) {
      outer_nodes <- setdiff(nodes, inner_nodes)
    } else {
      inner_nodes <- setdiff(nodes, outer_nodes)
    }
  }

  if (any(length(inner_nodes) < 1, length(outer_nodes) < 1)) {
    stop("`inner_nodes` or `outer_nodes` is zero length.", call. = FALSE)
  }

  if (!set_identical(c(inner_nodes, outer_nodes), nodes)) {
    stop("Length of `inner_nodes` and `outer_nodes` is not same as nodes.",
         call. = FALSE)
  }

  a <- calc_degree(start, end)
  start <- a$start / 180 * pi
  end <- a$end / 180 * pi

  if (r0 > r1) {
    temp <- r0
    r0 <- r1
    r1 <- temp
  }

  tt <- seq(end, start, length.out = length(inner_nodes) + 1)[-1]
  tt <- tt + 0.5 * (end - start) / length(inner_nodes)
  tt2 <- seq(end, start, length.out = length(outer_nodes) + 1)[-1]
  tt2 <- tt2 + 0.5 * (end - start) / length(outer_nodes)

  nodes_meta <- data.frame(.angle = c(tt, tt2),
                           .r = c(rep_len(r0, length.out = length(inner_nodes)),
                                  rep_len(r1, length.out = length(outer_nodes))),
                           .start = start,
                           .end = end,
                           .isInner = rep(c(TRUE, FALSE),
                                          times = c(length(inner_nodes),
                                                    length(outer_nodes))))
  ids <- ids(c(inner_nodes, outer_nodes), nodes)
  nodes <- cbind(igraph::as_data_frame(g, "vertices"),
                 reorder_by_ids(nodes_meta, ids))

  angle <- rlang::set_names(nodes$.angle, nodes$name)
  r <- rlang::set_names(nodes$.r, nodes$name)

  edges_meta <- data.frame(.angle0 = angle[edges$from],
                           .angle1 = angle[edges$to],
                           .r0 = r[edges$from],
                           .r1 = r[edges$to])
  edges <- cbind(edges, edges_meta)

  g <- igraph::graph_from_data_frame(d = edges,
                                     directed = igraph::is.directed(g),
                                     vertices = nodes)
  structure(g, class = c("bipartite_circular_graph", class(g)))
}
