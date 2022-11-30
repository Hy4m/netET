#' @title Magic labels of nodes
#' @description layer function to draw node labels.
#' @inheritParams linkET::geom_magic_text
#' @return a gg object.
#' @rdname geom_node_magic_text
#' @author Hou Yun
#' @export
geom_node_magic_text <- function(mapping = NULL,
                                 data = NULL,
                                 position = "identity",
                                 geom = "text",
                                 parse = FALSE,
                                 ...) {
  base <- ggplot2::aes_string(x = "x", y = "y", label = "name")
  mapping <- aes_modify(base, mapping)
  params <- list(data = data,
                 mapping = mapping,
                 stat = ggraph::StatFilter,
                 position = position,
                 geom = geom,
                 parse = parse,
                 inherit.aes = FALSE,
                 ...)
  do.call(linkET::geom_magic_text, params)
}

#' @title Extra nodes shaping
#' @description layer function to draw node makers.
#' @inheritParams linkET::geom_shaping
#' @return a gg object.
#' @rdname geom_node_shaping
#' @author Hou Yun
#' @export
geom_node_shaping <- function(mapping = NULL,
                              data = NULL,
                              position = "identity",
                              ...) {
  base <- ggplot2::aes_string(x = "x", y = "y")
  mapping <- aes_modify(base, mapping)
  params <- list(data = data,
                 mapping = mapping,
                 stat = ggraph::StatFilter,
                 position = position,
                 inherit.aes = FALSE,
                 ...)
  do.call(linkET::geom_shaping, params)
}

