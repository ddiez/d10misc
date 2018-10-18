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

}
