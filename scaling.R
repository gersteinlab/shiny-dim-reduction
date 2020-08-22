# This script has helper functions that enable data scaling
# before the data undergoes a dimensionality reduction.
# Criteria: samples are rows, features are columns
# Criteria: all entries are numeric, data is in a matrix

library(Rtsne)

# --------------
# USER VARIABLES
# --------------

source("~/Justin-Tool/code/build.R")

setwd(dep_loc)
categories_full <- readRDS("categories_full.rds")
decorations <- readRDS("decorations.rds")
pc_cap <- readRDS("pc_cap.rds")
order_total <- readRDS("order_total.rds")
perplexity_types <- readRDS("perplexity_types.rds")
amazon_keys <- readRDS("amazon_keys.rds")

# set keys
Sys.setenv("AWS_ACCESS_KEY_ID" = amazon_keys[1],
           "AWS_SECRET_ACCESS_KEY" = amazon_keys[2])
aws_bucket <- amazon_keys[3]

# create categories
cat_groups <- lapply(categories_full, function(x){names(x)})
name_cat <- unlist(cat_groups)
num_cat <- length(name_cat)
categories <- unlist(categories_full, recursive=FALSE)
names(categories) <- name_cat
names(name_cat) <- NULL

# create category subsets panel
sub_groups <- my_empty_list(name_cat)

for (cat in name_cat)
  sub_groups[[cat]] <- "Total"

for (dec_group in decorations)
{
  for (good_cat in dec_group$Categories)
  {
    sub_groups[[good_cat]] <- c(sub_groups[[good_cat]], 
                                names(dec_group$Subsets)[-1])
  }
}

# creates an empty list for neighbors
perplexity_list <- my_empty_list(sprintf("P%s", perplexity_types))

# note that the working directory after sourcing is pro_loc
setwd(pro_loc)

# ---------
# FUNCTIONS
# ---------

# normalizes a dataset by global min, max to [0,1]
norm_global <- function(data)
{
  max <- max(data)
  min <- min(data)
  diff <- max - min
  if (is.na(diff) || is.nan(diff) || diff == 0)
    print("!!!")
  (data-min)/diff
}

# normalizes a dataset by each feature's min, max to [0,1]
norm_local <- function(data)
{
  for (j in 1:ncol(data))
  {
    num_unique <- length(unique(data[,j]))
    if (num_unique < 2)
      data[,j] <- 0.5
    else
      data[,j] <- norm_global(data[,j])
  }
    
  data
}

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

# gets a subset
get_my_subset <- function(decor, cat, subset)
{
  if (sub == "Total")
    return(NULL)
  
  for (dec_group in decor)
  {
    if (cat %in% dec_group$Categories)
    {
      ref <- dec_group$Subsets$Reference
      ind <- dec_group$Subsets[[sub]]
      return(ref[ind])
    }
  }
  
  return(NULL)
}

# performs scaling
do_scal <- function(sca, scaled)
{
  if (sca == "Logarithmic")
    return(log_scale(scaled))
  return(scaled)
}

# performs normalization
do_norm <- function(nor, scaled)
{
  if (nor == "Normalized")
    return(norm_local(scaled))
  else
    return(norm_global(scaled))
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