# This file tests red_methods.R.

# -----
# SETUP
# -----

if (!exists("ran_install"))
{
  if (file.exists("install.R"))
    source("install.R")
  else
    stop("Could not confirm installation. Please source install.R manually.")
}

source_sdr("red_methods.R")
source_sdr("plotting.R")

test_table <- matrix(0, nrow = 100, ncol = 10)

for (i in 1:nrow(test_table))
  for (j in 1:ncol(test_table))
    test_table[i,j] <- sample(1:100/100, 1)

sprintf_clean("Is this a valid table?: %s", valid_table(test_table))
test_pca <- table_to_pca(test_table, 2)
test_vae <- table_to_vae(test_table)
test_umap <- table_to_umap(test_table, 2, 10)
test_phate <- table_to_phate(test_table, 2, 10)
test_tsne <- table_to_tsne(test_table, 2, 10)

plotly_2d(test_pca$x[,1], test_pca$x[,2])
plotly_2d(test_vae$predict[,1], test_vae$predict[,2])
plotly_2d(test_umap$layout[,1], test_umap$layout[,2])
plotly_2d(test_phate$embedding[,1], test_phate$embedding[,2])
plotly_2d(test_tsne$Y[,1], test_tsne$Y[,2])
