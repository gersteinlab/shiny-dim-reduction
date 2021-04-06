# This file tests perform_tsne.R.

# -----
# SETUP
# -----

project_name <- "exRNA"
setwd(sprintf("%s/shiny-dim-reduction", Sys.getenv("SHINY_DIM_REDUCTION_ROOT")))
source("perform_tsne.R", encoding="UTF-8")
source("pipeline.R", encoding="UTF-8")


test_loc <- "%s/PCA/PCA_100_Global Min-Max_Logarithmic_Total_miRNA.rds"
explore <- readRDS(sprintf(test_loc, pro_loc))
nei <- 30

start <- my_timer()
test1 <- my_rTSNE(explore, 2, nei, max_iter = 50)
print(my_timer(start))

start <- my_timer()
test2 <- my_rTSNE(explore, 2, nei, max_iter = 500)
print(my_timer(start))

start <- my_timer()
test3 <- my_rTSNE(explore, 2, nei, max_iter = 5000)
print(my_timer(start))

start <- my_timer()
test4 <- my_rTSNE(explore, 2, 10, max_iter = 100)
print(my_timer(start))

start <- my_timer()
test5 <- my_rTSNE(explore, 2, 100, max_iter = 100)
print(my_timer(start))

start <- my_timer()
test6 <- my_rTSNE(explore, 2, 1000, max_iter = 100)
print(my_timer(start))
