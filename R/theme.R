#' theme_base
#'
#' A tweaked ggthemes::theme_base().
#'
#' @param base_size base size for text.
#' @param ... arguments passed down to theme_dark().
#'
#' @export
theme_base <- function(base_size = 12,...) {
  ggthemes::theme_base(base_size = base_size, ...) +
    theme(plot.background = element_rect(color = NA))
}


#' theme_kuro
#'
#' A tweaked ggplot2::theme_dark().
#'
#' @param base_size base size for text.
#' @param ... arguments passed down to theme_dark().
#'
#' @export
theme_kuro <- function(base_size = 16, ...) {
  ggplot2::theme_dark(base_size = base_size, ...) +
    theme(
      plot.background = element_rect(fill = "grey50", color = "grey50"),
      legend.background = element_rect(fill = "grey50"),
      legend.text = element_text(color = "grey30") ,
      legend.title = element_text(color = "grey30"),
      axis.title = element_text(color = "grey30"))
}

#' rotate_xlab
#'
#' Sets element_text() values for axis.text.x so that horizontal x-labels
#' are perpendicular and nicely aligned with the x-axis.
#'
#' @param angle angle to rotate x labels.
#' @param hjust horizontal justification.
#' @param vjust vertical justification.
#' @param ... arguments passed down to element_text().
#'
#' @export
rotate_xlab <- function(angle = 90, hjust = 1, vjust = .5, ...) {
  theme(
    axis.text.x = element_text(angle = angle, hjust = hjust, vjust = vjust, ...)
  )
}

#' remove_ticks
#'
#' @param type the axes to remove the ticks from.
#'
#' @export
remove_ticks <- function(type = c("x", "y")) {
  x <- paste0("axis.ticks.", type)
  y <- replicate(length(x), element_blank())
  names(y) <- x
  do.call(theme, y)
}
