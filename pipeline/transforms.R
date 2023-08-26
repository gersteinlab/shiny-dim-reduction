# The purpose of this file is to do common matrix transformations.
# Note: all functions have matrix inputs / outputs.

if (!exists("sdr_config") || sdr_config$mode != "pipeline")
  source("app/install.R")
stopifnot(sdr_config$mode == "pipeline")

library(limma)

# -------------
# NORMALIZATION
# -------------

#' normalizes data to have a minimum of 0 and a maximum of 1
#'
#' @param data [vector, matrix, data.frame] not checked
#' @returns [vector, matrix, data.frame]
norm_min_max <- function(data)
{
  data_max <- max(data)
  data_min <- min(data)
  if (data_max == data_min)
  {
    data[] <- 0.5
    return(data)
  }
  (data - data_min) / (data_max - data_min)
}

#' normalizes data to have a mean of 0 and a standard deviation of 1
#'
#' @param data [vector, matrix, data.frame] not checked
#' @returns [vector, matrix, data.frame]
norm_z_score <- function(data)
{
  data_mean <- mean(data)
  data_sd <- sd(data)
  if (data_sd == 0)
  {
    data[] <- 0
    return(data)
  }
  (data - data_mean) / data_sd
}

#' normalizes each feature of data with norm_min_max
#'
#' @param data [vector, matrix, data.frame] not checked
#' @returns [vector, matrix, data.frame]
local_min_max <- function(data)
{
  apply(data, 2, norm_min_max)
}

#' normalizes each feature of data with norm_z_score
#'
#' @param data [vector, matrix, data.frame] not checked
#' @returns [vector, matrix, data.frame]
local_z_score <- function(data)
{
  apply(data, 2, norm_z_score)
}

#' performs normalization
#'
#' @param nor [string]
#' @param data [vector, matrix, data.frame] not checked
#' @returns [vector, matrix, data.frame]
do_norm <- function(nor, data)
{
  if (nor == "Global Min-Max")
    return(norm_min_max(data))
  if (nor == "Local Min-Max")
    return(local_min_max(data))
  if (nor ==  "Global Z-Score")
    return(norm_z_score(data))
  if (nor == "Local Z-Score")
    return(local_z_score(data))
  if (nor == "Quantile")
    return(t(limma::normalizeQuantiles(t(data))))
  stop_f("Invalid normalization mode: %s", nor)
}

# -------
# SCALING
# -------

#' logarithmically scales data
#'
#' @param data [vector, matrix, data.frame] not checked
#' @returns [vector, matrix, data.frame]
log_scale <- function(data)
{
  log2(data + 1)
}

#' performs scaling
#'
#' @param sca [string]
#' @param data [vector, matrix, data.frame] not checked
#' @returns [vector, matrix, data.frame]
do_scal <- function(sca, data)
{
  if (sca == "Linear")
    return(data)
  if (sca == "Logarithmic")
    return(log_scale(data))
  stop_f("Invalid normalization mode: %s", sca)
}

cat_f("SCA / NOR SETUP TIME: %.1f (sec)\n", net_time())
