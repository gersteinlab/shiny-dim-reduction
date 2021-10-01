# This file tests perform_tsne.R.

# -----
# SETUP
# -----

setwd(sprintf("%s/shiny-dim-reduction", Sys.getenv("SHINY_DIM_REDUCTION_ROOT")))
source("perform_tsne.R", encoding="UTF-8")
source("plotting.R", encoding="UTF-8")

project_name <- "exRNA"
source("pipeline.R", encoding="UTF-8")

test_loc <- "%s/PCA/PCA_100_Global Min-Max_Logarithmic_Total_miRNA.rds"
explore <- readRDS(sprintf(test_loc, pro_loc))$x
order_total <- readRDS(sprintf("%s/order_total.rds", dep_loc))
metadata <- order_total$miRNA$CONDITION

start <- my_timer()
test1 <- my_rTSNE(explore, 2, 30, max_iter = 50)
plotly_2d(test1$Y[,1], test1$Y[,2], metadata)
print(my_timer(start))

start <- my_timer()
test2 <- my_rTSNE(explore, 2, 30, max_iter = 500)
plotly_2d(test2$Y[,1], test2$Y[,2], metadata)
print(my_timer(start))

start <- my_timer()
test3 <- my_rTSNE(explore, 2, 30, max_iter = 5000)
plotly_2d(test3$Y[,1], test3$Y[,2], metadata)
print(my_timer(start))

start <- my_timer()
test4 <- my_rTSNE(explore, 2, 3, max_iter = 100)
plotly_2d(test4$Y[,1], test4$Y[,2], metadata)
print(my_timer(start))

start <- my_timer()
test5 <- my_rTSNE(explore, 2, 30, max_iter = 100)
plotly_2d(test5$Y[,1], test5$Y[,2], metadata)
print(my_timer(start))

start <- my_timer()
test6 <- my_rTSNE(explore, 2, 300, max_iter = 100)
plotly_2d(test6$Y[,1], test6$Y[,2], metadata)
print(my_timer(start))
