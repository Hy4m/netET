#' @title Select OTUs Based on Occurence and Abundance
#' @description Select otus based on occurence and abundance.
#' @param otu OTUs table, should be a data.frame or matrix.
#' @param otu_on_rows logical, if TRUE means OTUs are on rows, and samples are
#' on columns.
#' @param occur_thres threshold value of occurence.
#' @param abund_thres threshold value of abundance.
#' @param reverse provides a indicator of positive or negative selection.
#' @return OTUs table.
#' @author Hou Yun
#' @rdname select_otus
#' @export
select_otus <- function(otu,
                        otu_on_rows = TRUE,
                        occur_thres = 0.5,
                        abund_thres = 0.001,
                        reverse = FALSE) {
  if (isTRUE(otu_on_rows)) {
    otu <- as.data.frame(t(otu))
  } else {
    otu <- as.data.frame(otu)
  }

  not_all_is_zero <- vapply(otu, sum, numeric(1)) > 0
  otu <- otu[not_all_is_zero]
  if (empty(otu)) {
    stop("'otu' table is empty or all otus is zero.", call. = FALSE)
  }

  if (isFALSE(reverse) || is.null(reverse)) {
    reverse_occur <- reverse_abund <- FALSE
  } else {
    if (isTRUE(reverse)) {
      reverse_occur <- reverse_abund <- TRUE
    } else {
      reverse <- tolower(reverse)
      if (any(grepl("abund", reverse, fixed = TRUE))) {
        reverse_abund <- TRUE
      } else {
        reverse_abund <- FALSE
      }

      if (any(grepl("occur", reverse, fixed = TRUE))) {
        reverse_occur <- TRUE
      } else {
        reverse_occur <- FALSE
      }
    }
  }

  if (!is.null(occur_thres)) {
    occur <- colSums(otu > 0, na.rm = TRUE)/nrow(otu)

    if (isFALSE(reverse_occur)) {
      id_occur <- occur >= occur_thres
    } else {
      id_occur <- occur <= occur_thres
    }
    otu <- otu[, id_occur, drop = FALSE]
  }

  if (!is.null(abund_thres)) {
    abund <- colSums(otu, na.rm = TRUE)/sum(otu, na.rm = TRUE)

    if (isFALSE(reverse_abund)) {
      id_abund <- abund >= abund_thres
    } else {
      id_abund <- abund <= abund_thres
    }
    otu <- otu[, id_abund, drop = FALSE]
  }

  otu
}
