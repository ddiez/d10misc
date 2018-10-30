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
