#' theme_base
#'
#' A tweaked ggthemes::theme_base().
#'
#' @param base_size base size for text.
#' @param base_family base family for text.
#'
#' @export
theme_base <- function(base_size = 12, base_family = "") {
  ggthemes::theme_base(base_size = base_size, base_family = base_family) + theme(plot.background = element_rect(color = NA))
}
