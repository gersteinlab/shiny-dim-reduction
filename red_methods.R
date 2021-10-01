# The purpose of this file is to store all methods for reduction of a provided table.
# By a table, we mean an object of class matrix and array that contains numerics for each cell.

library(Rtsne)
library(reticulate)
library(umap)
library(tfruns)
library(tensorflow)
library(keras)

# ---------------
# GENERAL METHODS
# ---------------

# refuse to have less than 2 rows initially, since a point cannot have generalizable features
# refuse to have less than 4 rows initially, since then you can plot straight away
valid_table <- function(cand_table)
{
  if (!all.equal(class(matrix()), class(cand_table)))
    return(FALSE)

  if (nrow(cand_table) < 2 || ncol(cand_table) < 4)
    return(FALSE)

    for (j in 1:ncol(cand_table))
      if (!is.numeric(cand_table[,j]))
        return(FALSE)


   &&
    nrow(cand_table) > 0 && ncol(cand_table) > 3 &&
    is.numeric.matrix(cand_table)
}

# -----------
# PCA METHODS
# -----------

table_to_pca <- function(table, dim)
{
  pca <- stats::prcomp(scaled, center = TRUE, rank. = pc_cap)
  pca$rotation <- NULL
  pca$center <- NULL
  pca
}

pca_to_summary <- function()



# -----------
# VAE METHODS
# -----------



# ------------
# UMAP METHODS
# ------------

reduction_umap <- function(data, dim, perp)
{
  umap::umap(
    data,
    method = "umap-learn",
    n_neighbors = perp,
    n_components = dim,
    metric = 'euclidean',
    n_epochs = 500,
    min_dist = 0.1,
    input = "data",
    init = "random",
    verbose = TRUE,
    random_state = 0,
    transform_state = 0
  )
}

# -------------
# PHATE METHODS
# -------------


# ------------
# SETS METHODS
# ------------

# ------------
# tSNE METHODS
# ------------

# rTSNE with parameters set for speed on an m x n matrix
# dims (d): the number of final features
# perlexity (p): the perplexity (nearest neighbors) for tSNE
# max_iter (i): how many iterations the algorithm should run for
# theta (t): tradeoff between exact O(n^2) and Barnes-Hut approximation O(n log n)
# eta (e): learning rate
# momentum (u): velocity of convergence
# verbose (v): whether input is displayed as tSNE is run
# time complexity from tests: O(m^2 n i (d + p))
my_rTSNE <- function(data, dim = 2, perp = 1, max_iter = 500, theta = 0.5,
                     eta = 200, momentum = 0.5, verbose = FALSE)
{
  perplexity <- min(perp, floor((nrow(data)-1)/3))
  set.seed(42)

  Rtsne(data, dims = dim, perplexity = perp, max_iter = max_iter, theta = theta,
        eta = eta, momentum = momentum, final_momentum = momentum, verbose = verbose,
        initial_dims = ncol(data), exaggeration_factor = 1, num_threads = 1,
        stop_lying_iter = 0, mom_switch_iter = 0,
        check_duplicates = FALSE, pca = FALSE, partial_pca = FALSE,
        is_distance = FALSE, Y_init = NULL,
        pca_center = FALSE, pca_scale = FALSE, normalize = FALSE)
}
