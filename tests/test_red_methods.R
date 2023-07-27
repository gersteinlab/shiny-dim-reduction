# This file tests red_methods.R.

# -----
# SETUP
# -----

source("app/install.R")
if (!require(testthat))
  stop("Missing package: testthat")

source("pipeline/red_methods.R")
source("app/plotting.R")

# ------
# TESTER
# ------

# from a table, generates a list of reductions (and plots),
# raising an error if the original table is not valid.
# -- tsne (tsne_p)
# -- pca
# -- -- pca_e (pca_e_p)
# -- -- pca_s (pca_s_p)
# -- -- pca_t (pca_t_p)
# -- vae
# -- -- vae_e (vae_e_p)
# -- -- vae_s (vae_s_p)
# -- -- vae_t (vae_t_p)
# -- umap
# -- -- umap_e (umap_e_p)
# -- -- umap_s (umap_s_p1, umap_s_p2)
# -- -- umap_t (umap)
# -- phate (phate_p)
# -- sets, sets_labels, sets_frac (sets_p1, sets_p2)
# note: results$time returns the time elapsed in seconds
all_reductions <- function(table, labels)
{
  start <- Sys.time()
  results <- list()
  stopifnot(is_table(table))
  stopifnot(length(labels) == nrow(table))
  stopifnot(none_na(labels))

  # test tsne
  results$tsne <- table_to_tsne(table, 2, 10)
  results$tsne_p <- plotly_2d(results$tsne[,1], results$tsne[,2], labels)

  # test pca
  results$pca <- table_to_pca(table, 10)
  results$pca_e <- pca_to_explore(results$pca)
  results$pca_s <- pca_to_summary(results$pca)
  results$pca_t <- pca_to_tsne(results$pca, 2, 10)
  results$pca_e_p <- plotly_2d(results$pca_e[,1], results$pca_e[,2], labels)
  results$pca_s_p <- plotly_pca_sum(results$pca_s)
  results$pca_t_p <- plotly_2d(results$pca_t[,1], results$pca_t[,2], labels)

  # test vae
  results$vae <- table_to_vae(table, 2, 20) # batch size 20
  results$vae_e <- vae_to_explore(results$vae)
  results$vae_s <- vae_to_summary(results$vae)
  results$vae_t <- vae_to_tsne(results$vae, 2, 10)
  results$vae_e_p <- plotly_2d(results$vae_e[,1], results$vae_e[,2], labels)
  results$vae_s_p <- plotly_vae_sum(results$vae_s)
  results$vae_t_p <- plotly_2d(results$vae_t[,1], results$vae_t[,2], labels)

  # test umap
  results$umap <- table_to_umap(table, 2, 10)
  results$umap_e <- umap_to_explore(results$umap)
  results$umap_s <- umap_to_summary(results$umap)
  results$umap_t <- umap_to_tsne(results$umap, 2)
  results$umap_e_p <- plotly_2d(results$umap_e[,1], results$umap_e[,2], labels)
  results$umap_s_p1 <- plotly_heatmap_variance(knn_label_matrix(results$umap_s, labels))
  results$umap_s_p2 <- plotly_heatmap_dendrogram(knn_label_matrix(results$umap_s, labels))
  results$umap_t_p <- plotly_2d(results$umap_t[,1], results$umap_t[,2], labels)

  # test phate
  results$phate <- table_to_phate(table, 2, 10)
  results$phate_p <- plotly_2d(results$phate[,1], results$phate[,2], labels)

  # test sets
  results$sets <- table_to_sets(table, 0.4)
  results$sets_labels <- set_label_matrix(results$sets, labels)
  results$sets_frac <- results$sets_labels %>% truncate_rows() %>%
    sort_row_sums() %>% set_f1_f2(c(0, 1), c(0, 5))
  results$sets_p1 <- plotly_heatmap_variance(results$sets_frac)
  results$sets_p2 <- plotly_heatmap_dendrogram(results$sets_frac)

  results$time <- time_diff(start)
  results
}

# -------
# INVALID
# -------

tryCatch(all_reductions(data.frame(), NULL),
         error = function(e){cat("Caught error!\n")})

# ------
# RANDOM
# ------

# get random data
test_table <- matrix(0, nrow = 400, ncol = 20)
test_labels <- as.character(rep(1:5, each = 80))

for (i in 1:nrow(test_table))
  for (j in 1:ncol(test_table))
    test_table[i,j] <- runif(1, min = 0, max = 0.5 + as.numeric(test_labels[i]) / 10)

colnames(test_table) <- sprintf("Component %s", 1:20)

# test random data
test_red <- all_reductions(test_table, test_labels)

# -----------
# DUPLICATION
# -----------

dup_table <- matrix(c(rep(0, 6000), runif(2000, min = 0, max = 1)),
                    nrow = 400, ncol = 20, byrow = TRUE)
dup_labels <- as.character(rep(1:8, each = 50))
colnames(dup_table) <- sprintf("Component %s", 1:20)
dup_red <- all_reductions(dup_table, dup_labels)

# ----------
# ALL ZEROES
# ----------

zero_table <- matrix(0, nrow = 400, ncol = 20)
zero_labels <- as.character(rep(1:8, each = 50))
colnames(zero_table) <- sprintf("Component %s", 1:20)
zero_red <- all_reductions(zero_table, zero_labels)

# -----------
# MIRNA / RBP
# -----------

source_sdr("sca_nor_fun.R")
workflow_name <- "exRNA"
source_sdr("converter.R")

setwd(dep_loc)
order_total <- readRDS("order_total.rds")

mirna_labels <- order_total$miRNA$CONDITION
rbp_labels <- order_total$RNA_binding_proteins$CONDITION

setwd(com_loc)

combined_miRNA <- readRDS("combined_miRNA.rds")
scaled_miRNA <- combined_miRNA[,ind_sd_top(combined_miRNA, 100),drop=FALSE]
mirna_table <- norm_min_max(log_scale(scaled_miRNA))

combined_RBP <- readRDS("combined_RNA_binding_proteins.rds")
rbp_table <- norm_min_max(log_scale(combined_RBP))

# test miRNA data
mirna_red <- all_reductions(mirna_table, mirna_labels)

# test RBP data
rbp_red <- all_reductions(rbp_table, rbp_labels)

# test <- table_to_vae(rbp_table, 2, 20, verbose = 2)
# test <- table_to_tsne(rbp_table, 2, 20)
# test <- table_to_umap(rbp_table, 2, 20)
# rbp_sum <- umap_to_summary(test)
# hmm <- knn_label_matrix(rbp_sum, rbp_labels)

# -----
# TIMES
# -----

sprintf_clean("Time elapsed: %s (random)", test_red$time)
sprintf_clean("Time elapsed: %s (mirna)", mirna_red$time)
sprintf_clean("Time elapsed: %s (rbp)", rbp_red$time)

# ----------------
# SETS SCRATCHWORK
# ----------------

# tab1 <- function(lookup, ncol_final)
# {
#   label_freqs <- rep(0, ncol_final)
#   for (a in lookup)
#     label_freqs[a] <- label_freqs[a] + 1
#   label_freqs
# }
#
# tab2 <- function(lookup, ncol_final)
# {
#   tabulate(lookup, ncol_final)
# }
#
# # clearly tabulate is faster than for loops
# test1 <- sample(1:100, 10000000, replace = TRUE)
# system.time(tab1(test1, 102))
# system.time(tab2(test1, 102))
# sprintf_clean("Are they the same? %s", all.equal(tab1(test1, 102), tab2(test1, 102)))
#
# # clearly summary(Matrix()) is faster than which()
# sets_mat <- table_to_sets(mirna_table, 0.4)
# sm <- cbind(sets_mat, sets_mat, sets_mat, sets_mat, sets_mat)
# smm <- rbind(sm, sm, sm, sm)
# system.time(d1 <- summary(Matrix(smm, sparse = TRUE)))
# system.time(d2 <- which(smm == 1, arr.ind = TRUE))

workflow_name <- "exRNA"
source_sdr("converter.R")
source_sdr("sca_nor_fun.R")

setwd(raw_loc)
setwd("RBP_S2")
test <- readRDS("com_ZRANB2_K562.rds")
get_dependency("order_total")
rbp_order <- match_metadata_to_samples(sprintf("%s.metadata.tsv", test[,1]), order_total$miRNA, "DOWNLOAD_NAME")
hmm <- norm_min_max(log_scale(apply(test[,-1], 2, as.numeric)))
hmm2 <- hmm[,ind_sd_top(hmm, 100)]
valid_inds <- !grepl(".tsv", rbp_order$CONDITION)
result <- all_reductions(hmm2[valid_inds,], rbp_order$CONDITION[valid_inds])

