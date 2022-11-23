#' @title Coerce to a Graph
#' @description Functions to coerce a object to graph if possible.
#' @param x any \code{R} object.
#' @param diag logical, if TRUE will keep the diagonal of adjacency matrix data.
#' @param simplify if TRUE, Simple graphs are graphs which do not contain loop
#' and multiple edges.
#' @param directed whether or not to create a directed graph.
#' @param ... other parameters.
#' @return  a graph object.
#' @importFrom igraph as.igraph
#' @importFrom tidygraph as_tbl_graph
#' @author Hou Yun
#' @rdname network
as.igraph.cor_md_tbl <- function(x,
                                 ...,
                                 diag = FALSE,
                                 simplify = TRUE,
                                 directed = FALSE) {
  if (inherits(x, "grouped_md_tbl")) {
    stop("Cannot convert a grouped_md_tbl in igraph.", call. = FALSE)
  }
  rnm <- linkET::row_names(x)
  cnm <- linkET::col_names(x)
  x <- dplyr::filter(x, ...)

  if (isFALSE(diag)) {
    x <- linkET::trim_diag(x)
  }

  if (isTRUE(simplify)) {
    if (isTRUE(directed)) {
      x <- x[!duplicated(x), , drop = FALSE]
    } else {
      x <- linkET::trim_duplicate(x, diag = !diag)
    }
    nodes <- unique(c(x$.rownames, x$.colnames))
  } else {
    nodes <- unique(c(rnm, cnm))
  }

  igraph::graph_from_data_frame(x, directed = directed, vertices = nodes)
}



#' @rdname network
as.igraph.correlate <- function(x,
                                ...,
                                diag = FALSE,
                                simplify = TRUE,
                                directed = FALSE) {
  as.igraph(x = linkET::as_md_tbl(x),
            ...,
            diag = diag,
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
