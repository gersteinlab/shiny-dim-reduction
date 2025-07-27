# The purpose of this file is to integrate methods for
# standardized and efficient dimensionality reduction.

# COPY THE FOLDER rpytools
# sys <- import("sys", convert = TRUE)
# sys$path
# paste0(system.file(package = "reticulate"),"/python")

if (!exists("sdr_config") || sdr_config$mode != "pipeline")
  source("app/install.R")
stopifnot(sdr_config$mode == "pipeline")

# depends on Anaconda
library(reticulate)
library(tensorflow)
library(keras)
library(phateR)

# does not depend on Anaconda
library(umap)
library(Rtsne)
library(Matrix)

# should reduction methods be verbose by default?
verbose_red <- FALSE

# -----------
# tSNE METHOD
# -----------

#' fast t-Distributed Stochastic Neighbor Embedding (tSNE)
#'
#' @param table [table]
#' @param dim [int] in 1:3, final feature count
#' @param per [int] perplexity being the expected number of
#'    nearest neighbors in the embedding per sample
#' @param max_iter [int] number of iterations to run for
#' @param theta [num] tradeoff in [0, 1] between exact O(n^2)
#'    and Barnes-Hut approximation O(n log n)
#' @param eta [num] positive learning rate
#' @param momentum [num] positive velocity of convergence
#' @param verbose [boolean] whether to run verbosely
#' @returns [table]
table_to_tsne <- function(table, dim = 2L, per = 1L, max_iter = 500L, theta = 0.5,
                          eta = 200, momentum = 0.5, verbose = verbose_red)
{
  stopifnot(
    is_table(table),
    perplexity_is_valid(per, nrow(table))
  )
  cat_f("tSNE: (%d, %d > %d)\n", nrow(table), ncol(table), dim)
  set.seed(0)

  Rtsne(
    table, dims = dim, perplexity = per, max_iter = max_iter, theta = theta,
    eta = eta, momentum = momentum, final_momentum = momentum, verbose = verbose,
    initial_dims = ncol(table), exaggeration_factor = 1, num_threads = 1,
    stop_lying_iter = 0, mom_switch_iter = 0,
    check_duplicates = FALSE, pca = FALSE, partial_pca = FALSE,
    is_distance = FALSE, Y_init = NULL,
    pca_center = FALSE, pca_scale = FALSE, normalize = FALSE
  )$Y
}

# -----------
# PCA METHODS
# -----------

#' unscaled Principal Component Analysis (PCA)
#'
#' @param table [table]
#' @param dim [int] greater than 0, final feature count
#' @returns [list] with sdev [numeric], scale [boolean], x [table]
table_to_pca <- function(table, dim = 2L)
{
  stopifnot(is_table(table))
  cat_f("PCA: (%d, %d > %d)\n", nrow(table), ncol(table), dim)
  pca <- stats::prcomp(table, center = TRUE, rank. = dim)
  pca$rotation <- NULL
  pca$center <- NULL
  pca
}

#' access reduced table from PCA
#'
#' @param pca_result [list] not checked
#' @returns [table]
pca_to_explore <- function(pca_result)
{
  pca_result$x
}

#' variance captured vs number of components from PCA
#'
#' @param pca_result [list] not checked
#' @returns [data.frame]
pca_to_summary <- function(pca_result)
{
  eigs <- pca_result$sdev^2
  dim_seq <- seq_len(ncol(pca_result$x))
  props <- cumsum(eigs / sum(eigs))

  data.frame(
    "Components" = dim_seq,
    "Variance" = props[dim_seq]
  )
}

#' perform tSNE on reduced table from PCA
#'
#' @param pca_result [list] not checked
#' @param dim [int]
#' @param per [int]
#' @returns [table]
pca_to_tsne <- function(pca_result, dim = 2L, per = 1L)
{
  table_to_tsne(pca_to_explore(pca_result), dim, per)
}

# -----------
# VAE METHODS
# -----------

# must be done immediately after loading Keras
tensorflow::use_condaenv(
  "C:\\Users\\justin\\miniconda3\\envs\\r-reticulate"
)

# copy src: C:\Users\justin\Desktop\LibsR\reticulate\python
# copy dst: C:\Users\justin\miniconda3\envs\r-reticulate\Lib
# py_eval("1+1")

K <- keras::backend()

# disable eager execution
if (tensorflow::tf$executing_eagerly())
  tensorflow::tf$compat$v1$disable_eager_execution()

# ensure reproducibility
tensorflow::tf$random$set_seed(0)

#' The reason we are using small layers
#' is to improve scalability.
#' Let A be the number of input neurons.
#' Let d1 be the number of neurons in layer 1.
#' Let d2 be the number of neurons in layer 2.
#' Let B be the number of latent neurons.
#'
#' @param input_dim [int] A
#' @param latent_dim [int] B
#' @returns [integer] c(d1, d2)
get_intermediate <- function(input_dim, latent_dim)
{
  stopifnot(input_dim >= latent_dim)
  dim_d1 <- input_dim^0.5 * latent_dim^0.5
  dim_d2 <- dim_d1^0.5 * latent_dim^0.5
  dim_d1 <- min(input_dim, floor(dim_d1))
  dim_d2 <- min(input_dim, floor(dim_d2))
  c(dim_d1, dim_d2)
}

#' stops the training after a fixed number of epochs
#' where validation loss (val_loss) does not decrease
#'
#' @param patience [int]
#' @param verbose [int] level of verbosity in 0:1
#' @returns [object]
pat_callback <- function(patience = 10L, verbose = 1)
{
  callback_early_stopping(
    monitor = "val_loss", mode = "min", verbose = verbose,
    patience = patience, restore_best_weights = TRUE
  )
}

# stops the training if an exploding loss gradient occurs
naan_callback <- callback_terminate_on_naan()

# records the history of all iterations
loss <- numeric()
record_loss <- function(batch, logs)
{
  loss <<- c(loss, logs$loss)
}
record_callback <- callback_lambda(on_batch_end = record_loss)

#' perform fitting for a Variational Autoencoder (VAE)
#'
#' @param vae [model] not checked
#' @param x_train [vector, matrix, array] not checked
#' @param batch_size [int] samples per batch;
#'    lower batch size takes more time / less memory
#' @param patience [int]
#' @param epochs [int] upper bound on final epoch number
#' @param verbose [int] level of verbosity in 0:2
#' @returns [history]
vae_fit <- function(vae, x_train, batch_size = 2L,
                    patience = 10L, epochs = 1000L, verbose = 2)
{
  fit(
    vae,
    x = x_train,
    y = x_train,
    shuffle = TRUE,
    batch_size = batch_size,
    epochs = epochs,
    validation_split = 0.2,
    verbose = verbose,
    callbacks = list(
      pat_callback(patience, min(verbose, 1)),
      naan_callback,
      record_callback
    )
  )
}

#' Variational Autoencoding (VAE)
#'
#' @param table [table] in [0, 1]
#' @param dim [int] greater than 0, final feature count
#' @param batch_size [int] samples per batch;
#'    lower batch size takes more time / less memory
#' @param patience [int]
#' @param epochs [int] upper bound on final epoch number
#' @param verbose [int] level of verbosity in 0:2
#' @returns [list] with history [history], predict [table], loss [numeric]
table_to_vae <- function(table, dim = 2L, batch_size = 2L, patience = 10L,
                         epochs = 1000L, verbose = ifelse(verbose_red, 2, 0))
{
  # require all entries to be within [0, 1]
  stopifnot(
    is_table(table),
    vec_between(table, 0, 1)
  )

  num_samp <- nrow(table)
  input_dim <- ncol(table)
  latent_dim <- dim

  dims <- get_intermediate(input_dim, latent_dim)
  dim_d1 <- dims[1]
  dim_d2 <- dims[2]
  cat_f(
    "VAE: (%d, %d > %d > %d > %d)\n",
    num_samp, input_dim, dim_d1, dim_d2, dim
  )

  x_inputs <- layer_input(shape = input_dim)
  enc_d1 <- layer_dense(x_inputs, dim_d1, activation = "relu")
  enc_d2 <- layer_dense(enc_d1, dim_d2, activation = "relu")

  z_dec_d2 <- layer_dense(units = dim_d2, activation = "relu")
  d2_dec_d1 <- layer_dense(units = dim_d1, activation = "relu")
  d1_dec_means <- layer_dense(units = input_dim, activation = "sigmoid")

  z_mean <- layer_dense(enc_d2, latent_dim)
  z_log_var <- layer_dense(enc_d2, latent_dim)
  z_combine <- layer_concatenate(list(z_mean, z_log_var))

  # sampling with variation
  sampling <- function(arg)
  {
    seq_latent <- seq_len(latent_dim)
    z_mean <- arg[, seq_latent, drop = FALSE]
    z_log_var <- arg[, seq_latent + latent_dim, drop = FALSE]

    epsilon <- k_random_normal(
      shape = c(k_shape(z_mean)[[1]]), # not sure if c() is needed
      mean = 0.0,
      stddev = 1.0
    )

    z_mean + k_exp(z_log_var / 2) * epsilon
  }

  # inputs-derived z, from taking variables in inputs space as inputs
  z_inputs <- z_combine %>% layer_lambda(sampling)
  means_inputs <- z_inputs %>% z_dec_d2() %>% d2_dec_d1() %>% d1_dec_means()

  # latent-derived z, from taking variables in latent space as inputs
  z_latent <- layer_input(shape = latent_dim)
  means_latent <- z_latent %>% z_dec_d2() %>% d2_dec_d1() %>% d1_dec_means()

  # inputs-derived x to inputs-derived means: autoencoder
  vae <- keras_model(x_inputs, means_inputs)

  # inputs-derived x to inputs-derived z means: encoder
  encoder <- keras_model(x_inputs, z_inputs)

  # latent-derived z to latent-derived means: generator
  generator <- keras_model(z_latent, means_latent)

  # compile with the customized vae loss function
  vae_loss <- function(x, x_decoded_mean)
  {
    xent_loss <- input_dim / 1.0 * loss_binary_crossentropy(x, x_decoded_mean)
    kl_prep_vec <- z_log_var - k_square(z_mean) - k_exp(z_log_var)
    kl_loss <- k_mean(kl_prep_vec, axis = -1L)
    xent_loss + 1 - 0.5 * kl_loss
  }

  # adam is faster and more accurate than rmsprop
  # amsgrad fixes a mathematical hole in the convergence
  # use gradient clipping to prevent an explosion
  # set learning rate low so the batch doesn't explode
  pref_comp <- optimizer_adam(learning_rate = 0.0001, amsgrad = TRUE, clipnorm = 0.1)

  vae %>% keras::compile(optimizer = pref_comp,
                         loss = vae_loss,
                         experimental_run_tf_function = FALSE)

  # reset loss before fitting
  loss <<- numeric()

  history <- vae_fit(vae, table, batch_size, patience, epochs, verbose)
  predict <- predict(encoder, table, batch_size)

  # minimize memory leaks
  k_clear_session()

  list(
    "history" = history,
    "predict" = predict,
    "loss" = loss
  )
}

#' access reduced table from VAE
#'
#' @param vae_result [list] not checked
#' @returns [table]
vae_to_explore <- function(vae_result)
{
  vae_result$predict
}

#' training loss vs validation loss from VAE
#'
#' @param vae_result [list] not checked
#' @returns [data.frame]
vae_to_summary <- function(vae_result)
{
  loss <- vae_result$loss
  val_loss <- vae_result$history$metrics$val_loss
  num_losses <- length(loss)
  num_val_losses <- length(val_loss)

  # ensure you have even divisibility
  stopifnot(num_losses > 0, num_val_losses > 0, num_losses %% num_val_losses == 0)

  # note: colnames are not as safe as one might hope
  data.frame(
    "Training Iterations" = rep(seq_len(num_losses), 2),
    "Loss Value" = c(loss, rep(val_loss, each = num_losses / num_val_losses)),
    "Loss Type" = rep(c("Testing Loss", "Validation Loss"), each = num_losses),
    check.names = FALSE
  )
}

#' perform tSNE on reduced table from VAE
#'
#' @param vae_result [list] not checked
#' @param dim [int]
#' @param per [int]
#' @returns [table]
vae_to_tsne <- function(vae_result, dim = 2L, per = 1L)
{
  table_to_tsne(vae_to_explore(vae_result), dim, per)
}

# ------------
# UMAP METHODS
# ------------

#' Uniform Manifold Approximation and Projection (UMAP)
#' note: UMAP perturbs duplicate samples before reducing
#'
#' @param table [table]
#' @param dim [int] in 1:3, final feature count
#' @param per [int] perplexity being the expected number of
#'    nearest neighbors in the embedding per sample
#' @param verbose [boolean] whether to run verbosely
#' @returns [list] with layout [matrix], knn [list], config [list]
table_to_umap <- function(table, dim = 2L, per = 1L, verbose = verbose_red)
{
  stopifnot(
    is_table(table),
    perplexity_is_valid(per, nrow(table))
  )
  cat_f("UMAP: (%d, %d > %d)\n", nrow(table), ncol(table), dim)

  result <- umap::umap(
    table,
    method = "naive",
    n_neighbors = per,
    n_components = dim,
    metric = 'euclidean',
    n_epochs = 500,
    min_dist = 0.1,
    input = "data",
    init = "random",
    verbose = verbose,
    random_state = 0,
    transform_state = 0
  )

  # avoid duplicating original data
  result$data <- NULL
  result
}

#' access reduced table from UMAP
#'
#' @param umap_result [list] not checked
#' @returns [table]
umap_to_explore <- function(umap_result)
{
  umap_result$layout
}

#' nearest neighbors for each sample from UMAP
#'
#' @param umap_result [list] not checked
#' @returns [matrix]
umap_to_summary <- function(umap_result)
{
  umap_result$knn$indexes
}

#' perform tSNE on reduced table from UMAP
#' note: tSNE reuses the perplexity given to UMAP
#'
#' @param umap_result [list]
umap_to_tsne <- function(umap_result, dim = 2L)
{
  table_to_tsne(
    umap_to_explore(umap_result),
    dim,
    umap_result$config$n_neighbors
  )
}

# -------------
# PHATE METHODS
# -------------

#' make a matrix of samples from the normal distribution
#'
#' @param row_n [int] not checked
#' @param col_n [int] not checked
#' @returns [matrix]
rnorm_mat <- function(row_n, col_n)
{
  matrix(rnorm(row_n * col_n), nrow = row_n, ncol = col_n)
}

#' perturbs a matrix by a weighted rnorm_mat
#'
#' @param mat [matrix] not checked
#' @param weight [num] not checked
#' @returns [matrix]
perturb_mat <- function(mat, weight = 0.000000001)
{
  mat + weight * rnorm_mat(nrow(mat), ncol(mat))
}

#' perturbs a matrix if duplicate rows exist
#'
#' @param mat [matrix]
#' @param weight [num] not checked
#' @returns [matrix]
perturb_if_dup <- function(mat, weight = 0.000000001)
{
  stopifnot(is.matrix(mat))
  if (!anyDuplicated(mat))
    return(mat)
  perturb_mat(mat, weight)
}

#' Potential of Heat diffusion for Affinity-based Transition Embedding (PHATE)
#' note: PHATE does not work correctly with duplicate rows
#' note: only the embedding is kept for storage efficiency
#'
#' @param table [table]
#' @param dim [int] in 1:3, final feature count
#' @param per [int] perplexity being the expected number of
#'    nearest neighbors in the embedding per sample
#' @param verbose [boolean] whether to run verbosely
#' @returns [matrix]
table_to_phate <- function(table, dim = 2L, per = 1L, verbose = verbose_red) {
  stopifnot(
    is_table(table),
    perplexity_is_valid(per, nrow(table))
  )
  cat_f("PHATE: (%d, %d > %d)\n", nrow(table), ncol(table), dim)

  result <- phateR::phate(
    perturb_if_dup(table), ndim = dim, knn = per, decay = 40, n.landmark = 2000,
    gamma = 1, t = "auto", mds.solver = "sgd", knn.dist.method = "euclidean",
    init = NULL, mds.method = "metric", mds.dist.method = "euclidean",
    t.max = 100, npca = 10, verbose = verbose, n.jobs = 1, seed = 0)
  result$embedding
}

# ------------
# Sets METHODS
# ------------

#' target[i, j] returns whether data[i, j] >= threshold,
#' removing columns with no values at the threshold or above
#'
#' @param table [table] in [0, 1]
#' @param threshold [num] in [0, 1]
#' @returns [matrix]
table_to_sets <- function(table, threshold) {
  # require all entries and the threshold to be within [0, 1]
  # because all Sets should undergo Global Min-Max normalization
  stopifnot(
    is_table(table),
    vec_between(table, 0, 1),
    vec_between(threshold, 0, 1)
  )
  cat_f("Sets: (%d, %d) >= %.4f\n", nrow(table), ncol(table), threshold)

  target <- matrix(as.numeric(table >= threshold),
                   nrow = nrow(table),
                   dimnames = dimnames(table))
  target[, colSums(target) > 0, drop = FALSE]
}

#' given a binary matrix mat from calculate_sets,
#' let final[feature, label] be the fraction of samples
#' with that label where that feature was present in mat
#'
#' @param sets_result [matrix] with column names and binary entries
#' @param labels [character] for each sample in sets_result
#' @returns [matrix]
set_label_matrix <- function(sets_result, labels){
  stopifnot(
    is.matrix(sets_result),
    sets_result %in% 0:1,
    is_str(labels, nrow(sets_result)),
    !anyNA(labels),
    is_str(colnames(sets_result), ncol(sets_result))
  )

  rownames_final <- colnames(sets_result)
  rowlen_final <- length(rownames_final)
  set_types <- unique(labels)
  num_types <- length(set_types)
  cat_f("Sets: (%d, %d) to (%d, %d)\n",
        nrow(table), ncol(table), rowlen_final, num_types)

  summary <- summary(Matrix(sets_result, sparse = TRUE))
  summary_i <- summary[, 1]
  summary_j <- summary[, 2]

  final <- rep(0, rowlen_final*num_types)
  lookup <- match(labels, set_types)
  lookup2 <- (lookup - 1) * rowlen_final

  for (len in seq_along(summary_i))
  {
    num_i <- summary_i[len]
    index <- lookup2[num_i] + summary_j[len]
    final[index] <- final[index] + 1
  }

  final <- matrix(final, ncol = num_types)
  numbers <- tabulate(lookup, num_types)

  for (j in seq_len(num_types))
    final[, j] <- final[, j] / numbers[j]

  dimnames(final) <- list(rownames_final, set_types)
  final
}

cat_f("REDUCTION SETUP TIME: %.1f (sec)\n", net_time())
