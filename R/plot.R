#' plot_heatmap
#'
#' @param x object to plot.
#' @param row.cluster logical; whether to cluster rows.
#' @param col.cluster logical; whether to cluster columns
#' @param scale logical; whether to scale rows.
#' @param row.name name for the row variable.
#' @param col.name name for the col variable.
#' @param value.name name for the value variable.
#' @param ... arguments passed down to methods.
#'
#' @export
plot_heatmap <- function(x, ...) {
  UseMethod("plot_heatmap")
}

#' @rdname plot_heatmap
#' @export
plot_heatmap.matrix <- function(x, row.cluster = TRUE, col.cluster = FALSE, scale = TRUE, row.name = "row", col.name = "col", value.name = "value", ...) {
  if (row.cluster) {
    h <- hclust(as.dist(1 - cor(t(x))))
    x <- x[h$order, ]
  }

  if (col.cluster) {
    h <- hclust(as.dist(1 - cor(x)))
    x <- x[, h$order]
  }

  if (scale)
    x <- t(scale(t(x)))

  d <- x %>% as.data.frame() %>%
    rownames_to_column(row.name) %>%
    gather(!!col.name, !!value.name, - .data[[row.name]])

  d[[row.name]] <- factor(d[[row.name]], levels = rownames(x))
  d[[col.name]] <- factor(d[[col.name]], levels = colnames(x))

  ggplot(d, aes_string(x = col.name, y = row.name, fill = value.name)) +
    geom_tile() +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0))
}
