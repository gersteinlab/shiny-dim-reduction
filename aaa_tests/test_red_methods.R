# This file tests red_methods.R.

# -----
# SETUP
# -----

setwd(sprintf("%s/shiny-dim-reduction", Sys.getenv("SHINY_DIM_REDUCTION_ROOT")))
source("red_methods.R", encoding="UTF-8")

numeric_table <- matrix(1:1000, ncol = 10)
printis.numeric()
