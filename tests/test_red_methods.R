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

start <- my_timer()
test_umap <- table_to_umap(test_table, 2, 10)
print(my_timer(start))
precomputed_knn <- test_umap$knn
start <- my_timer()
test_umap_knn <- umap(
  test_table,
  method = "naive",
  n_neighbors = 10,
  n_components = 2,
  metric = 'euclidean',
  n_epochs = 500,
  min_dist = 0.1,
  input = "data",
  init = "random",
  verbose = TRUE,
  random_state = 0,
  transform_state = 0,
  knn = precomputed_knn
)
print(my_timer(start))

library(uwot)

start <- my_timer()
test_umap_knn <- uwot::umap(
  test_table,
  n_neighbors = 10,
  n_components = 2,
  metric = 'euclidean',
  n_epochs = 500,
  min_dist = 0.1,
  init = "random",
  approx_pow = TRUE,
  verbose = TRUE
)
print(my_timer(start))

plotly_2d(test_umap$layout[,1], test_umap$layout[,2])

test_phate <- table_to_phate(test_table, 2, 10)
plotly_2d(test_phate$embedding[,1], test_phate$embedding[,2])

test_tsne <- table_to_tsne(test_table, 2, 10)
plotly_2d(test_tsne$Y[,1], test_tsne$Y[,2])

# idea for UMAP summary: see what proportion of nearest neighbors are of the same type
# ex:
# BILE: BILE (98%), PLASMA (2%)
# SPUTUM: SPUTUM (66%), SPUTUM (20%), PLASMA (14%)
# vs original data!!!
# could be a heatmap; final - initial percentages



library(FNN)
before <- get.knn(test_table, 5)$nn.index
test_umap <- table_to_umap(test_table, 2, 10)
after <- get.knn(test_umap$layout, 5)$nn.index
labels <- rep(1:5, each = 80)

heatmat_before <- matrix(0, nrow = 5, ncol = 5)
for (i in 1:nrow(before))
{
  for (j in 1:ncol(before))
  {
    heatmat_before[labels[i], labels[before[i,j]]] <- heatmat_before[labels[i], labels[before[i,j]]] + 1
  }
}

plotly_heatmap_variance(heatmat_before, color_seq(5, "Inferno"), smooth = FALSE)

heatmat_after <- matrix(0, nrow = 5, ncol = 5)

for (i in 1:nrow(after))
{
  for (j in 1:ncol(after))
  {
    heatmat_after[labels[i], labels[after[i,j]]] <- heatmat_after[labels[i], labels[after[i,j]]] + 1
  }
}

plotly_heatmap_variance(heatmat_after, color_seq(5, "Inferno"), smooth = FALSE)
