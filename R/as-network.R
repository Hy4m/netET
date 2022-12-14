#' @title Coerce to a Graph
#' @description Functions to coerce a object to graph if possible.
#' @param x any \code{R} object.
#' @param simplify if TRUE, will return a simplified graph which do not contain
#' loop and multiple edges.
#' @param directed whether or not to create a directed graph.
#' @param ... other parameters.
#' @return  a graph object.
#' @importFrom igraph as.igraph
#' @importFrom tidygraph as_tbl_graph
#' @author Hou Yun
#' @rdname network
as.igraph.cor_md_tbl <- function(x,
                                 ...,
                                 simplify = TRUE,
                                 directed = FALSE) {
  if (inherits(x, "grouped_md_tbl")) {
    stop("Cannot convert a grouped_md_tbl in igraph.", call. = FALSE)
  }
  rnm <- linkET::row_names(x)
  cnm <- linkET::col_names(x)
  x <- dplyr::filter(x, ...)

  if (!is.function(simplify)) {
    if (is.null(simplify)) {
      simplify <- FALSE
    }
    if (is.character(simplify)) {
      simplify <- match.arg(simplify, c("edges", "nodes", "vertices", "all", "none"))
      simplify <- switch (simplify,
                          "edges" = TRUE,
                          "nodes" = "vertices",
                          "vertices" = "vertices",
                          "none" = FALSE)
    }
  }
  g <- igraph::graph_from_data_frame(x,
                                     directed = directed,
                                     vertices = c(rnm, cnm))
  if (is.logical(simplify)) {
    if (isTRUE(simplify)) {
      g <- igraph::simplify(g)
    }
  } else if (is.function(simplify)) {
    g <- simplify(g)
  } else {
    nodes <- igraph::V(g)$name
    degree <- igraph::degree(g)
    nodes <- nodes[degree < 1]
    if (length(nodes) > 0) {
      g <- igraph::delete_vertices(g, nodes)
    }
  }

  g
}

#' @rdname network
as.igraph.correlate <- function(x,
                                ...,
                                simplify = TRUE,
                                directed = FALSE) {
  as.igraph(x = linkET::as_md_tbl(x),
            ...,
            simplify = simplify,
            directed = directed)
}

#' @rdname network
as.igraph.mantel_tbl <- function(x, ...) {
  as.igraph(linkET::as_md_tbl(x), ...)
}

#' @rdname network
as.igraph.easycorrelation <- function(x, ...) {
  as.igraph(linkET::as_correlate(x), ...)
}

#' @rdname network
as.igraph.rcorr <- function(x, ...) {
  as.igraph(linkET::as_correlate(x), ...)
}

#' @rdname network
as.igraph.corr.test <- function(x, ...) {
  as.igraph(linkET::as_correlate(x), ...)
}

#' @rdname network
as_tbl_graph.cor_igraph <- function(x, ...) {
  x <- as_tbl_graph(as.igraph(x, ...))
  class(x) <- unique(c("cor_tbl_graph", class(x)))
  x
}

#' @rdname network
as_tbl_graph.cor_md_tbl <- function(x, ...) {
  as_tbl_graph(as.igraph(x, ...))
}

#' @rdname network
as_tbl_graph.correlate <- function(x, ...) {
  as_tbl_graph(as.igraph(x, ...))
}

#' @rdname network
as_tbl_graph.mantel_tbl <- function(x, ...) {
  as_tbl_graph(as.igraph(x, ...))
}

#' @rdname network
as_tbl_graph.easycorrelation <- function(x, ...) {
  as_tbl_graph(as.igraph(x, ...))
}

#' @rdname network
as_tbl_graph.rcorr <- function(x, ...) {
  as_tbl_graph(as.igraph(x, ...))
}

#' @rdname network
as_tbl_graph.corr.test <- function(x, ...) {
  as_tbl_graph(as.igraph(x, ...))
}
