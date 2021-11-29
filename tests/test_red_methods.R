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
plotly_heatmap_variance(h2-h1, color_seq(5, "Inferno"), smooth = FALSE)

plotly_2d(test_umap$layout[,1], test_umap$layout[,2])

test_phate <- table_to_phate(test_table, 2, 10)
plotly_2d(test_phate$embedding[,1], test_phate$embedding[,2])

test_tsne <- table_to_tsne(test_table, 2, 10)
plotly_2d(test_tsne$Y[,1], test_tsne$Y[,2])




before <- get.knn(test_table, 5)$nn.index
test_umap <- table_to_umap(test_table, 2, 10)
after <- test_umap$knn$indexes
labels <- rep(1:5, each = 80)

plotly_heatmap_variance(heatmat_before, color_seq(5, "Inferno"), smooth = FALSE)

heatmat_after <- matrix(0, nrow = 5, ncol = 5)

setwd(pro_loc)

pre_image <- readRDS("combined/combined_miRNA.rds")
post_umap <-  table_to_umap(pre_image, 2, 10)

start <- get.knn(pre_image)$nn.index
end <- get.knn(post_umap$layout)$nn.index
setwd(dep_loc)
order_total <- readRDS("order_total.rds")
labels <- order_total$miRNA$BIOFLUID

h1 <- make_heatmat(start, labels)
h2 <- make_heatmat(end, labels)
plotly_heatmap_variance(h2-h1, color_seq(5, "Inferno"), smooth = FALSE)

plotly_heatmap_variance(heatmat_after, color_seq(5, "Inferno"), smooth = FALSE)
