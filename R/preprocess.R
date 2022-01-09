# This script has helper functions that enable data scaling
# before the data undergoes a dimensionality reduction.
# Criteria: samples are rows, features are columns
# Criteria: all entries are numeric, data is in a matrix
# source("scaling.R", encoding="UTF-8")

if (!exists("ran_install"))
{
  if (file.exists("install.R"))
    source("install.R")
  else
    stop("Could not confirm installation. Please source install.R manually.")
}

source_sdr("workflows.R")
source_sdr("storage.R")
source_sdr("sca_nor_fun.R")

# --------------
# USER VARIABLES
# --------------

# open from dependencies
get_dependency("order_total", empty_named_list(name_cat))
get_dependency("perplexity_types")
get_dependency("pc_cap")

# create categories and subsets
init_cat()
init_sub(names)

# note that the working directory after sourcing is pro_loc
setwd(pro_loc)

perplexity_list <- empty_named_list(perplexity_types)
