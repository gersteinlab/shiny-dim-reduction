# This script contains functions for t-Distributed Stochastic Neighbor Embedding.

library(Rtsne)

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

# id for perplexity of final embedding
id_perp <- function(perp)
{
  sprintf("P%s", perp)
}

# id for dimension of final embedding
id_dim <- function(tsne)
{
  sprintf("TSNE%s", tsne)
}
