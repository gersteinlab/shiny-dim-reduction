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

test_table <- matrix(0, nrow = 400, ncol = 20)

for (i in 1:nrow(test_table))
  for (j in 1:ncol(test_table))
    test_table[i,j] <- runif(1)

sprintf_clean("Is this a valid table?: %s", valid_table(test_table))

test_pca <- table_to_pca(test_table, 10)
test_pca_sum <- pca_to_summary(test_pca)
plotly_2d(test_pca$x[,1], test_pca$x[,2])
plotly_pca_sum(test_pca_sum)

test_vae <- table_to_vae(test_table, 2, 10)
test_vae_sum <- vae_to_summary(test_vae)
plotly_2d(test_vae$predict[,1], test_vae$predict[,2])
plotly_vae_sum(test_vae_sum)

test_umap <- table_to_umap(test_table, 2, 10)
test_umap
plotly_2d(test_umap$layout[,1], test_umap$layout[,2])

test_phate <- table_to_phate(test_table, 2, 10)
plotly_2d(test_phate$embedding[,1], test_phate$embedding[,2])

test_tsne <- table_to_tsne(test_table, 2, 10)
plotly_2d(test_tsne$Y[,1], test_tsne$Y[,2])







