#' @title Special line layers
#' @description Special layers for circular layout.
#' @inheritParams ggplot2::geom_path
#' @return a layer object.
#' @author Hou Yun
#' @rdname geom_edge_circular
#' @importFrom ggplot2 aes_string
#' @export
geom_edge_circular <- function(mapping = NULL,
                               data = ggraph::get_edges(),
                               position = "identity",
                               ...,
                               show.legend = NA,
                               arrow = NULL,
                               lineend = "butt",
                               linejoin = "round",
                               linemitre = 10,
                               na.rm = FALSE) {
  mapping <- aes_modify(ggplot2::aes_string(.angle0 = ".angle0",
                                            .angle1 = ".angle1",
                                            .r0 = ".r0",
                                            .r1 = ".r1"),
                        mapping)
  mapping <- rename(mapping, "colour" = "color", "width" = "size")
  mapping <- rename(mapping,
                    "edge_colour" = "colour",
                    "edge_alpha" = "alpha",
                    "edge_width" = "width",
                    "edge_linetype" = "linetype")

  params <- list(...)
  params <- rename(params, "colour" = "color", "width" = "size")
  params <- rename(params,
                   "edge_colour" = "colour",
                   "edge_alpha" = "alpha",
                   "edge_width" = "width",
                   "edge_linetype" = "linetype")

  ggplot2::layer(data = data,
                 mapping = mapping,
                 geom = GeomEdgeCircular,
                 stat = "identity",
                 position = position,
                 show.legend = show.legend,
                 inherit.aes = FALSE,
                 params = list(arrow = arrow,
                               lineend = lineend,
                               linejoin = linejoin,
                               linemitre = linemitre,
                               na.rm = na.rm,
                               ...))
}

#' @name netET-extensions
#' @format NULL
#' @usage NULL
#' @export
GeomEdgeCircular <- ggplot2::ggproto(
  "GeomEdgeCircular", ggplot2::Geom,
  required_aes = c(".angle0", ".angle1", ".r0", ".r1"),
  setup_data = function(data, params) {
    if (empty(data)) {
      return(data)
    }

    extra <- setdiff(names(data), c(".angle0", ".angle1", ".r0", ".r1", "group"))
    pos <- lapply(seq_len(nrow(data)), function(ii) {
      row <- data[ii, , drop = FALSE]
      a <- seq(row$.angle0, row$.angle1, length.out = 100)
      r <- seq(row$.r0, row$.r1, length.out = 100)
      data.frame(x = r * cos(a), y = r * sin(a), group = ii)
    })
    pos <- do.call(rbind, pos)
    cbind(pos, data[pos$group, extra])
  },
  draw_panel = function(data,
                        panel_params,
                        coord,
                        arrow = NULL,
                        lineend = "butt",
                        linejoin = "round",
                        linemitre = 10,
                        na.rm = FALSE) {
    if (empty(data)) {
      return(grid::nullGrob())
    }
    names(data) <- gsub("edge_", "", names(data), fixed = TRUE)
    names(data) <- ifelse(names(data) == "width", "size", names(data))
    ggplot2::GeomPath$draw_panel(data = data,
                                 panel_params = panel_params,
                                 coord = coord,
                                 arrow = arrow,
                                 lineend = lineend,
                                 linejoin = linejoin,
                                 linemitre = linemitre,
                                 na.rm = na.rm)
  },
  draw_key = function(data, params, size) {
    grid::segmentsGrob(0.1, 0.5, 0.9, 0.5,
                 gp = grid::gpar(
                   col = scales::alpha(data$edge_colour, data$edge_alpha),
                   fill = scales::alpha(params$arrow.fill %||% data$edge_colour %||%
                                          data$edge_fill %||% "black", data$edge_alpha),
                   lwd = data$edge_width * ggplot2::.pt,
                   lty = data$edge_linetype,
                   lineend = "butt"
                 ),
                 arrow = params$arrow
    )
  },
  default_aes = ggplot2::aes(
    edge_colour = "black", edge_width = 0.5, edge_linetype = 1,
    edge_alpha = NA
  )
)

#' @title Special text layers
#' @description Special layers for circular layout.
#' @inheritParams ggplot2::geom_text
#' @param nice_facing If TRUE, text rotation angle will be automatically optimized.
#' @param expand gap between nodes and text label.
#' @return a layer object.
#' @author Hou Yun
#' @rdname geom_node_text_circular
#' @export
geom_node_text_circular <- function(mapping = NULL,
                                    data = NULL,
                                    position = "identity",
                                    ...,
                                    nice_facing = TRUE,
                                    expand = 0.02,
                                    parse = FALSE,
                                    show.legend = NA,
                                    na.rm = FALSE) {
  mapping <- aes_modify(ggplot2::aes_string(.angle = ".angle",
                                            .r = ".r",
                                            .isInner = ".isInner",
                                            label = "name"),
                        mapping)
  ggplot2::layer(data = data,
                 mapping = mapping,
                 stat = ggraph::StatFilter,
                 geom = GeomNodeTextCircular,
                 position = position,
                 show.legend = show.legend,
                 inherit.aes = FALSE,
                 params = list(parse = parse,
                               nice_facing = nice_facing,
                               expand = expand,
                               na.rm = na.rm,
                               ...))
}

#' @name netET-extensions
#' @format NULL
#' @usage NULL
#' @export
GeomNodeTextCircular <- ggplot2::ggproto(
  "GeomNodeTextCircular", ggplot2::Geom,
  required_aes = c(".angle", ".r", ".isInner", "label"),
  setup_data = function(data, params) {
    if (empty(data)) {
      return(data)
    }

    expand <- params$expand %||% 0.02
    nice_facing <- params$nice_facing %||% TRUE
    angle <- data$.angle
    extra <- setdiff(names(data), c(".angle", ".r", ".isInner"))
    data$x <- ifelse(data$.isInner, (data$.r - expand) * cos(angle),
                     (data$.r + expand) * cos(angle))
    data$y <- ifelse(data$.isInner, (data$.r - expand) * sin(angle),
                     (data$.r + expand) * sin(angle))
    angle <- (angle * 180 / pi) %% 360
    if (isTRUE(nice_facing)) {
      data$hjust <- ifelse(data$.isInner,
                           ifelse(angle > 270 | angle < 90, 1, 0),
                           ifelse(angle > 270 | angle < 90, 0, 1))
    } else {
      data$hjust <- ifelse(data$.isInner, 1, 0)
    }

    data$vjust <- 0.5
    data$angle <- calc_text_angle(angle, nice_facing)
    data$filter <- TRUE
    data[, setdiff(names(data), c(".angle", ".r", ".isInner")), drop = FALSE]
  },
  draw_panel = function(data,
                        panel_params,
                        coord,
                        nice_facing = TRUE,
                        expand = 0.02,
                        parse = FALSE,
                        na.rm = FALSE) {
    if (empty(data)) {
      return(grid::nullGrob())
    }

    ggplot2::GeomText$draw_panel(data = data,
                                 panel_params = panel_params,
                                 coord = coord,
                                 parse = parse,
                                 na.rm = na.rm)
  },
  draw_key = ggplot2::draw_key_text,
  default_aes = ggplot2::aes(
    colour = "black", size = 3.88, alpha = NA, family = "",
    fontface = 1, lineheight = 1.2
  )
)

#' @title Special annotate layers
#' @description Special layers to add arc-rect for circular layout.
#' @param start start angle in degree.
#' @param end end angle in degree.
#' @param r0 inner radius of arc-rect.
#' @param r1 outer radius of arc-rect.
#' @param ... other parameters passing to `geom_polygon()`.
#' @return a layer object.
#' @author Hou Yun
#' @rdname annotate_arc_rect
#' @export
annotate_arc_rect <- function(start = 180,
                              end = 360,
                              r0 = 0.6,
                              r1 = 1,
                              ...) {
  a <- calc_degree(start, end)
  start <- a$start / 180 * pi
  end <- a$end / 180 * pi
  if (r0 > r1) {
    temp <- r0
    r0 <- r1
    r1 <- temp
  }

  a <- c(start, start, end, end, start)
  r <- c(r0, r1, r1, r0, r0)
  a_new <- r_new <- NULL
  for (ii in 2:5) {
    if (ii == 5) {
      a_new <- c(a_new, seq(a[ii-1], a[ii], length.out = 100))
      r_new <- c(r_new, seq(r[ii-1], r[ii], length.out = 100))
    } else {
      a_new <- c(a_new, seq(a[ii-1], a[ii], length.out = 101)[-101])
      r_new <- c(r_new, seq(r[ii-1], r[ii], length.out = 101)[-101])
    }
  }
  df <- data.frame(x = cos(a_new) * r_new,
                   y = sin(a_new) * r_new)
  ggplot2::geom_polygon(mapping = aes_string(x = "x", y = "y"),
                        data = df,
                        ...,
                        inherit.aes = FALSE)
}

#' @noRd
calc_text_angle <- function (angle, nice_facing = TRUE) {
  if (isTRUE(nice_facing)) {
    angle <- ifelse(angle > 90 & angle < 270,
                     (angle + 180) %% 360, angle)
  }
  angle
}

