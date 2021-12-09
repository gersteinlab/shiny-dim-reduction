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

# -----
# TESTS
# -----

# get random data
test_table <- matrix(0, nrow = 400, ncol = 20)
test_labels <- as.character(rep(1:5, each = 80))

for (i in 1:nrow(test_table))
  for (j in 1:ncol(test_table))
    test_table[i,j] <- runif(1, min = 0, max = 0.5 + as.numeric(test_labels[i]) / 10)

colnames(test_table) <- sprintf("Component %s", 1:20)

# test random data
start1 <- my_timer()
sprintf_clean("Is this a valid table?: %s", valid_table(test_table))

# test tsne
test_tsne <- table_to_tsne(test_table, 2, 10)
plotly_2d(test_tsne$Y[,1], test_tsne$Y[,2], test_labels)

# test pca
test_pca <- table_to_pca(test_table, 10)
test_pca_explore <- pca_to_explore(test_pca)
test_pca_sum <- pca_to_summary(test_pca)
test_pca_tsne <- pca_to_tsne(test_pca, 2, 10)

plotly_2d(test_pca_explore[,1], test_pca_explore[,2], test_labels)
plotly_pca_sum(test_pca_sum)
plotly_2d(test_pca_tsne$Y[,1], test_pca_tsne$Y[,2], test_labels)

# test vae
test_vae <- table_to_vae(test_table, 2, 20) # batch size 20
test_vae_explore <- vae_to_explore(test_vae)
test_vae_sum <- vae_to_summary(test_vae)
test_vae_tsne <- vae_to_tsne(test_vae, 2, 10)

plotly_2d(test_vae_explore[,1], test_vae_explore[,2], test_labels)
plotly_vae_sum(test_vae_sum)
plotly_2d(test_vae_tsne$Y[,1], test_vae_tsne$Y[,2], test_labels)

# test umap
test_umap <- table_to_umap(test_table, 2, 10)
test_umap_explore <- umap_to_explore(test_umap)
test_umap_sum <- umap_to_summary(test_umap)
test_umap_tsne <- umap_to_tsne(test_umap, 2)

plotly_2d(test_umap_explore[,1], test_umap_explore[,2], test_labels)
plotly_heatmap_variance(knn_label_matrix(test_umap_sum, test_labels))
plotly_heatmap_dendrogram(knn_label_matrix(test_umap_sum, test_labels))
plotly_2d(test_umap_tsne$Y[,1], test_umap_tsne$Y[,2], test_labels)

# test phate
test_phate <- table_to_phate(test_table, 2, 10)
plotly_2d(test_phate$embedding[,1], test_phate$embedding[,2], test_labels)

# test sets
test_sets <- table_to_sets(test_table, 0.4)
test_sets_labels <- set_label_matrix(test_sets, test_labels)
test_sets_frac <- test_sets_labels %>% truncate_rows() %>% sort_row_sums() %>%
  set_f1_f2(c(0, 1), c(0, 5))
plotly_heatmap_variance(test_sets_frac)
plotly_heatmap_dendrogram(test_sets_frac)

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

# test tsne
mirna_tsne <- table_to_tsne(mirna_table, 2, 10)
plotly_2d(mirna_tsne$Y[,1], mirna_tsne$Y[,2], mirna_labels)

# test pca
mirna_pca <- table_to_pca(mirna_table, 10) # dim 10
mirna_pca_explore <- pca_to_explore(mirna_pca)
mirna_pca_sum <- pca_to_summary(mirna_pca)
mirna_pca_tsne <- pca_to_tsne(mirna_pca, 2, 25) # perplexity 25

plotly_2d(mirna_pca_explore[,1], mirna_pca_explore[,2], mirna_labels)
plotly_pca_sum(mirna_pca_sum)
plotly_2d(mirna_pca_tsne$Y[,1], mirna_pca_tsne$Y[,2], mirna_labels)

# test vae
mirna_vae <- table_to_vae(mirna_table, 10, 64) # dim 10, batch size 64
mirna_vae_explore <- vae_to_explore(mirna_vae)
mirna_vae_sum <- vae_to_summary(mirna_vae)
mirna_vae_tsne <- vae_to_tsne(mirna_vae, 2, 10)

plotly_2d(mirna_vae_explore[,1], mirna_vae_explore[,2], mirna_labels)
plotly_vae_sum(mirna_vae_sum)
plotly_2d(mirna_vae_tsne$Y[,1], mirna_vae_tsne$Y[,2], mirna_labels)

# test umap
mirna_umap <- table_to_umap(mirna_table, 10, 25) # dim 10, perplexity 25
mirna_umap_explore <- umap_to_explore(mirna_umap)
mirna_umap_sum <- umap_to_summary(mirna_umap)
mirna_umap_tsne <- umap_to_tsne(mirna_umap, 2)

plotly_2d(mirna_umap_explore[,1], mirna_umap_explore[,2], mirna_labels)
plotly_heatmap_variance(knn_label_matrix(mirna_umap_sum, mirna_labels))
plotly_heatmap_dendrogram(knn_label_matrix(mirna_umap_sum, mirna_labels))
plotly_2d(mirna_umap_tsne$Y[,1], mirna_umap_tsne$Y[,2], mirna_labels)

# test phate
mirna_phate <- table_to_phate(mirna_table, 2, 25) # perplexity 25
plotly_2d(mirna_phate$embedding[,1], mirna_phate$embedding[,2], mirna_labels)

# test sets
mirna_sets <- table_to_sets(mirna_table, 0.4)
mirna_sets_labels <- set_label_matrix(mirna_sets, mirna_labels)
mirna_sets_frac <- mirna_sets_labels %>% truncate_rows() %>% sort_row_sums() %>%
  set_f1_f2(c(0, 1), c(0, 32))
plotly_heatmap_variance(mirna_sets_frac)
plotly_heatmap_dendrogram(mirna_sets_frac)

end2 <- my_timer(start2)

# get RNA binding protein data
setwd(pro_loc)
combined_RBP <- readRDS("combined/combined_RNA_binding_proteins.rds")
rbp_table <- norm_min_max(log_scale(combined_RBP))
rbp_labels <- order_total$RNA_binding_proteins$CONDITION

# test RBP data
start3 <- my_timer()
sprintf_clean("Is this a valid table?: %s", valid_table(rbp_table))

# test tsne
rbp_tsne <- table_to_tsne(rbp_table, 2, 10)
plotly_2d(rbp_tsne$Y[,1], rbp_tsne$Y[,2], rbp_labels)

# test pca
rbp_pca <- table_to_pca(rbp_table, 10) # dim 10
rbp_pca_explore <- pca_to_explore(rbp_pca)
rbp_pca_sum <- pca_to_summary(rbp_pca)
rbp_pca_tsne <- pca_to_tsne(rbp_pca, 2, 25) # perplexity 25

plotly_2d(rbp_pca_explore[,1], rbp_pca_explore[,2], rbp_labels)
plotly_pca_sum(rbp_pca_sum)
plotly_2d(rbp_pca_tsne$Y[,1], rbp_pca_tsne$Y[,2], rbp_labels)

# test vae
rbp_vae <- table_to_vae(rbp_table, 10, 64) # dim 10, batch size 64
rbp_vae_explore <- vae_to_explore(rbp_vae)
rbp_vae_sum <- vae_to_summary(rbp_vae)
rbp_vae_tsne <- vae_to_tsne(rbp_vae, 2, 10)

plotly_2d(rbp_vae_explore[,1], rbp_vae_explore[,2], rbp_labels)
plotly_vae_sum(rbp_vae_sum)
plotly_2d(rbp_vae_tsne$Y[,1], rbp_vae_tsne$Y[,2], rbp_labels)

# test umap
rbp_umap <- table_to_umap(rbp_table, 10, 25) # dim 10, perplexity 25
rbp_umap_explore <- umap_to_explore(rbp_umap)
rbp_umap_sum <- umap_to_summary(rbp_umap)
rbp_umap_tsne <- umap_to_tsne(rbp_umap, 2)

plotly_2d(rbp_umap_explore[,1], rbp_umap_explore[,2], rbp_labels)
plotly_heatmap_variance(knn_label_matrix(rbp_umap_sum, rbp_labels))
plotly_heatmap_dendrogram(knn_label_matrix(rbp_umap_sum, rbp_labels))
plotly_2d(rbp_umap_tsne$Y[,1], rbp_umap_tsne$Y[,2], rbp_labels)

# test phate
rbp_phate <- table_to_phate(rbp_table, 2, 25) # perplexity 25
plotly_2d(rbp_phate$embedding[,1], rbp_phate$embedding[,2], rbp_labels)

# test sets
rbp_sets <- table_to_sets(rbp_table, 0.4)
rbp_sets_labels <- set_label_matrix(rbp_sets, rbp_labels)
rbp_sets_frac <- rbp_sets_labels %>% truncate_rows() %>% sort_row_sums() %>%
  set_f1_f2(c(0, 1), c(0, 32))
plotly_heatmap_variance(rbp_sets_frac)
plotly_heatmap_dendrogram(rbp_sets_frac)

end3 <- my_timer(start3)

# print out times
print(end1)
print(end2)
print(end3)
