# This script has helper functions that enable data scaling
# before the data undergoes a dimensionality reduction.
# Criteria: samples are rows, features are columns
# Criteria: all entries are numeric, data is in a matrix
# source("scaling.R", encoding="UTF-8")

setwd(sprintf("%s/shiny-dim-reduction", Sys.getenv("SHINY_DIM_REDUCTION_ROOT")))
source("pipeline.R", encoding="UTF-8")
source("perform_tsne.R", encoding="UTF-8")

library(limma)

# --------------
# USER VARIABLES
# --------------
setwd(app_loc)

# open from dependencies
get_from_dir("categories_full")
get_from_dir("decorations")

get_from_dir("order_total")
get_from_dir("perplexity_types")
get_from_dir("pc_cap")

# create categories and subsets
init_cat()
init_sub(names)

# note that the working directory after sourcing is pro_loc
setwd(pro_loc)

perplexity_list <- my_empty_list(perplexity_types)

# -------------
# NORMALIZATION
# -------------

# normalizes a vector or matrix to [0,1]
norm_min_max <- function(data)
{
  max <- base::max(data)
  min <- base::min(data)
  if (max == min)
  {
    data[] <- 0.5
    return(data)
  }
  (data-min)/(max-min)
}

# normalizes a vector or matrix to have a mean of 0 and a variance of 1
norm_z_score <- function(data)
{
  mean <- base::mean(data)
  sd <- stats::sd(data)
  if (sd == 0)
  {
    data[] <- 0
    return(data)
  }
  (data-mean)/sd
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
    return(t(normalizeQuantiles(t(scaled))))
}

# ---------
# FUNCTIONS
# ---------

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

# uses only a fraction of features, sorted by variance
feature_start <- function(data, fraction)
{
  variances <- apply(data, 2, var)
  data <- data[,order(variances, decreasing=TRUE),drop=FALSE]
  num_features <- calc_feat(pc_cap, fraction, ncol(data))
  data[,1:num_features, drop=FALSE]
}

