#' to_tidy
#'
#' Convert a matrix, data.frame or tibble into a tidy tibble.
#'
#' @param x a matrix, data.frame or tibble object.
#' @param row.name name for row data.
#' @param col.name name for column data.
#' @param value.name name use for value column.
#' @param stringsAsFactors logical; whether to convert col/row names to factors (preserving original ordering).
#' @export
to_tidy <- function(x, ...) {
  UseMethod("to_tidy")
}

#' @rdname to_tidy
#' @export
to_tidy.matrix <- function(x, row.name = "row", ...) {
  if (is.null(rownames(x)))
    x <- as_tibble(x) %>% rownames_to_column(var = row.name)
  else
    x <- as_tibble(x, rownames = row.name)
  to_tidy(x, row.name = row.name, ...)
}

#' @rdname to_tidy
#' @export
to_tidy.data.frame <- function(x, row.name = "row", ...) {
  y <- as_tibble(x) %>% rownames_to_column(var = row.name)
  to_tidy(y, row.name = row.name, ...)
}


#' @rdname to_tidy
#' @export
to_tidy.tbl_df <- function(x, row.name = "row", col.name = "col", value.name = "value", stringsAsFactors = FALSE) {
  rn <- x[[row.name]]
  cn <- colnames(x)[-1]
  y <- x %>% gather(!!col.name, !!value.name, -!!row.name)

  if (stringsAsFactors) {
    y <- y %>%
      mutate(!!row.name := factor(.data[[row.name]], levels = rn)) %>%
      mutate(!!col.name := factor(.data[[col.name]], levels = cn))
  }
  y
}
