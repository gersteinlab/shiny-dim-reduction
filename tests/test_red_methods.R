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
project_name <- "exRNA"
source_sdr("preprocess.R")

# get random data
test_table <- matrix(0, nrow = 400, ncol = 20)
test_labels <- rep(1:5, each = 80)

for (i in 1:nrow(test_table))
  for (j in 1:ncol(test_table))
    test_table[i,j] <- runif(1, min = 0, max = 0.5 + test_labels[i] / 10)

# test random data
start1 <- my_timer()
sprintf_clean("Is this a valid table?: %s", valid_table(test_table))

test_pca <- table_to_pca(test_table, 10)
test_pca_sum <- pca_to_summary(test_pca)
plotly_2d(test_pca$x[,1], test_pca$x[,2], test_labels)
plotly_pca_sum(test_pca_sum)

test_vae <- table_to_vae(test_table, 2, 10)
test_vae_sum <- vae_to_summary(test_vae)
plotly_2d(test_vae$predict[,1], test_vae$predict[,2], test_labels)
plotly_vae_sum(test_vae_sum)

test_umap <- table_to_umap(test_table, 2, 10)
test_umap_sum <- umap_to_summary(test_umap)
plotly_2d(test_umap$layout[,1], test_umap$layout[,2], test_labels)
plotly_heatmap_variance(knn_label_matrix(test_umap_sum, test_labels), smooth = FALSE)

test_phate <- table_to_phate(test_table, 2, 10)
plotly_2d(test_phate$embedding[,1], test_phate$embedding[,2], test_labels)

test_tsne <- table_to_tsne(test_table, 2, 10)
plotly_2d(test_tsne$Y[,1], test_tsne$Y[,2], test_labels)
end1 <- my_timer(start1)

# get miRNA data
setwd(pro_loc)
combined_miRNA <- readRDS("combined/combined_miRNA.rds")

feature_start <- function(data, fraction)
{
  variances <- apply(data, 2, var)
  data <- data[,order(variances, decreasing=TRUE),drop=FALSE]
  num_features <- calc_feat(pc_cap, fraction, ncol(data))
  data[,1:num_features, drop=FALSE]
}

scaled_miRNA <- feature_start(combined_miRNA, 0.1)

mirna_table <- norm_min_max(log_scale(scaled_miRNA))
setwd(dep_loc)
order_total <- readRDS("order_total.rds")
mirna_labels <- order_total$miRNA$CONDITION

# test miRNA data
start2 <- my_timer()
sprintf_clean("Is this a valid table?: %s", valid_table(mirna_table))

mirna_pca <- table_to_pca(mirna_table, 10)
mirna_pca_sum <- pca_to_summary(mirna_pca)
plotly_2d(mirna_pca$x[,1], mirna_pca$x[,2], mirna_labels)
plotly_pca_sum(mirna_pca_sum)

mirna_vae <- table_to_vae(mirna_table, 2, 64)
mirna_vae_sum <- vae_to_summary(mirna_vae)
plotly_2d(mirna_vae$predict[,1], mirna_vae$predict[,2], mirna_labels)
plotly_vae_sum(mirna_vae_sum)

mirna_umap <- table_to_umap(mirna_table, 2, 10)
mirna_umap_sum <- umap_to_summary(mirna_umap)
plotly_2d(mirna_umap$layout[,1], mirna_umap$layout[,2], mirna_labels)
plotly_heatmap_variance(knn_label_matrix(mirna_umap_sum, mirna_labels), smooth = FALSE)

mirna_phate <- table_to_phate(mirna_table, 2, 10)
plotly_2d(mirna_phate$embedding[,1], mirna_phate$embedding[,2], mirna_labels)

mirna_tsne <- table_to_tsne(mirna_table, 2, 10)
plotly_2d(mirna_tsne$Y[,1], mirna_tsne$Y[,2], mirna_labels)
end2 <- my_timer(start2)

mirna_sets <- calculate_sets(mirna_table, 0.4)
mirna_gathered <- set_label_matrix(mirna_sets, mirna_labels)
mirna_gathered %>% truncate_rows() %>% sort_row_sums() %>%
  set_f1_f2(c(0, 1), c(0, 32)) %>% plotly_heatmap_variance(inferno, smooth = FALSE)

print(end1)
print(end2)
