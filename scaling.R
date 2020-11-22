# This script has helper functions that enable data scaling
# before the data undergoes a dimensionality reduction.
# Criteria: samples are rows, features are columns
# Criteria: all entries are numeric, data is in a matrix
# source("scaling.R", encoding="UTF-8")

setwd(sprintf("%s/shiny-dim-reduction", Sys.getenv("SHINY_DIM_REDUCTION_ROOT")))
source("pipeline.R", encoding="UTF-8")

library(limma)
library(Rtsne)

# --------------
# USER VARIABLES
# --------------
setwd(app_loc)

# essential
get_from_dir("categories_full", NULL)
get_from_dir("decorations", NULL)
get_from_dir("amazon_keys", NULL)
get_from_dir("order_total", NULL)

# optional
get_from_dir("perplexity_types", 1:5)
get_from_dir("pc_cap", 3)

assign_keys(amazon_keys)

# create categories
init_cat(categories_full)

# create subsets
sub_groups <- my_empty_list(name_cat)

for (cat in name_cat)
  sub_groups[[cat]] <- "Total"

for (dec_group in decorations)
  for (gc in dec_group$Categories)
    sub_groups[[gc]] <- c(sub_groups[[gc]], names(dec_group$Subsets)[-1])

# creates an empty list for neighbors
perplexity_list <- my_empty_list(sprintf("P%s", perplexity_types))

# note that the working directory after sourcing is pro_loc
setwd(pro_loc)

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

# uses only a fraction of features
feature_start <- function(data, fraction)
{
  features <- floor(fraction * (ncol(data)-pc_cap) + pc_cap)
  data[,1:features]
}

# performs scaling
do_scal <- function(sca, scaled)
{
  if (sca == "Logarithmic")
    return(log_scale(scaled))
  return(scaled)
}

# performs safe subsetting
get_safe_sub <- function(sub, df, dec, cat)
{
  if (sub == "Total")
    return(df)
  else
  {
    cols <- get_my_subset(dec, cat, sub)
    return(df[,colnames(df) %in% cols,drop=FALSE])
  }
}

# rTSNE on dim dimensions for tuning perplexity, iterations
my_rTSNE <- function(data, dim, perp) {
  max_perp <- floor((nrow(data)-1)/3)
  set.seed(42)
  Rtsne(data, dims = dim, perplexity = min(perp, max_perp), max_iter = 500,
        initial_dims = 10, stop_lying_iter = 250, mom_switch_iter = 250,
        theta = 0.5, momentum = 0.0, final_momentum = 0.8,  
        eta = 200, exaggeration_factor = 12, num_threads = 1,
        check_duplicates = FALSE, pca = FALSE, partial_pca = FALSE, 
        verbose = FALSE, is_distance = FALSE, Y_init = NULL, 
        pca_center = FALSE, pca_scale = FALSE, normalize = FALSE)
}