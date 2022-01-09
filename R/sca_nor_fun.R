# The purpose of this file is to store functions related to scaling and normalization.

if (!exists("ran_install"))
{
  if (file.exists("install.R"))
    source("install.R")
  else
    stop("Could not confirm installation. Please source install.R manually.")
}

library(limma)

# -------------
# NORMALIZATION
# -------------

# normalizes a vector or matrix to [0,1]
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

# normalizes a vector or matrix to have a mean of 0 and a variance of 1
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

# normalizes each feature with min-max
local_min_max <- function(data)
{
  apply(data, 2, norm_min_max)
}

# normalizes each feature with z-score
local_z_score <- function(data)
{
  apply(data, 2, norm_z_score)
}

# performs normalization
do_norm <- function(nor, scaled)
{
  if (nor == nor_options[1])
    return(norm_min_max(scaled))
  if (nor == nor_options[2])
    return(local_min_max(scaled))
  if (nor == nor_options[3])
    return(norm_z_score(scaled))
  if (nor == nor_options[4])
    return(local_z_score(scaled))
  if (nor == nor_options[5])
    return(t(limma::normalizeQuantiles(t(scaled))))
}

# -------
# SCALING
# -------

# logarithmically scales data
log_scale <- function(data)
{
  log2(data+1)
}

# performs scaling
do_scal <- function(sca, scaled)
{
  if (sca == "Logarithmic")
    return(log_scale(scaled))
  return(scaled)
}
