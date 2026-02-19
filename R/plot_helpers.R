#' Stack ggplot QAQC panels into a shared-axis plotly subplot
#'
#' Convenience wrapper around \code{plotly::subplot()} that accepts one
#' or more \pkg{ggplot2} objects, converts each to \pkg{plotly} via
#' \code{plotly::ggplotly()}, and stacks them vertically with a shared
#' x-axis.
#'
#' Currently not called by any function; available for interactive or
#' future use in multi-panel QAQC review plots.
#'
#' @param ... One or more \pkg{ggplot2} plot objects.
#' @param heights Numeric vector of relative row heights (length must
#'   equal the number of plots).  When \code{NULL} (default), panels are
#'   given equal height.
#' @param shareX Logical.  Share the x-axis across panels.  Default
#'   \code{TRUE}.
#' @param margin Numeric scalar.  Vertical margin between panels as a
#'   fraction of the total height.  Default \code{0.05}.
#'
#' @return A \pkg{plotly} \code{htmlwidget} containing the stacked
#'   subplots.
#'
#' @seealso \code{\link{plot_qaqc_timeseries}} (the package's main QAQC
#'   plotting function)
#'
#' @keywords internal
make_qaqc_subplot <- function(..., heights = NULL, shareX = TRUE, margin = 0.05) {
  plots <- list(...)
  n     <- length(plots)
  
  if (n == 0) stop("make_qaqc_subplot(): no plots supplied.", call. = FALSE)
  if (is.null(heights)) heights <- rep(1 / n, n)
  
  plotly::subplot(
    lapply(plots, function(g) suppressWarnings(plotly::ggplotly(g))),
    nrows   = n,
    heights = heights,
    shareX  = shareX,
    margin  = margin
  )
}
