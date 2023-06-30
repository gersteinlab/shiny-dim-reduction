# The purpose of this file is to validate numeric data files.

if (!exists("sdr_config") || sdr_config$mode != "pipeline")
  source("app/install.R")
stopifnot(sdr_config$mode == "pipeline")

#' whether x is a num_data object
#'
#' @param x An object.
#' @param row_n An integer (not checked).
#' @param col_n An integer (not checked).
#' @returns TRUE or FALSE.
is_num_data <- function(x, row_n, col_n)
{
  # must be a matrix
  is.matrix(x) &&
    # prevents NA, NaN, Inf, -Inf, non-numerics
    all_fin(x) &&
    # correct dimensions
    nrow(x) == row_n && ncol(x) == col_n &&
    # no rownames (tidyverse convention)
    is.null(rownames(x))
}
