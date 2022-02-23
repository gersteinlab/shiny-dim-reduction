# The purpose of this file is to store all methods for reduction of a provided table.
# By a table, we mean an object of class matrix and array that contains numerics for each cell.

if (!exists("ran_install"))
{
  if (file.exists("install.R"))
    source("install.R")
  else
    stop("Could not confirm installation. Please source install.R manually.")
}

# depends on Anaconda
library(reticulate)
library(tensorflow)
library(keras)
library(phateR)

# does not depend on Anaconda
library(umap)
library(Rtsne)
library(Matrix)

# should reduction methods be verbose?
verbose_red <- FALSE

# dimensions (dim): the final number of features for a reduction
# -- note that this differs from the com vs dim distinction in 1st round / 2nd round reductions
# perplexity (per): the expected number of nearest neighbors for each sample in an embedding
# verbose: whether updates are displayed as the analysis is run

# determines if a table is valid for dimensionality reduction
# note: anything of the form "combined_miRNA" should satisfy this
valid_table <- function(cand_table)
{
  if (!all.equal(class(matrix()), class(cand_table)))
    return(FALSE)

  row_n <- nrow(cand_table)
  col_n <- ncol(cand_table)

  # refuse to have less than 4 rows initially, since 3 points define a plane
  # refuse to have less than 4 columns initially, since then you can immediately plot on 3D
  # and plot on 2D for second-round reductions.
  if (row_n < 4 || col_n < 4)
    return(FALSE)

  if (sum(is.na(cand_table)) > 0) # NAs not allowed
    return(FALSE)

  if (sum(is.nan(cand_table)) > 0) # NaNs not allowed
    return(FALSE)

  # row names should be absent
  if (!is.null(rownames(cand_table)))
    return(FALSE)

  # column names are required
  if (length(colnames(cand_table)) != col_n)
    return(FALSE)

  for (j in seq_len(col_n))
    if (!is.numeric(cand_table[,j]))
      return(FALSE)

  TRUE
}

# determines if a data.frame is valid as metadata
valid_metadata <- function(cand_df)
{
  if (!all.equal(class(data.frame()), class(cand_df)))
    return(FALSE)

  row_n <- nrow(cand_df)
  col_n <- ncol(cand_df)

  # must have at least one row and one column
  if (row_n < 1 || col_n < 1)
    return(FALSE)

  if (sum(is.na(cand_df)) > 0) # NAs not allowed
    return(FALSE)

  if (sum(is.nan(as.matrix(cand_df))) > 0) # NaNs not allowed
    return(FALSE)

  TRUE
}

# ------------
# tSNE METHODS
# ------------

# t-Distributed Stochastic Neighbor Embedding (tSNE) with parameters prioritizing speed
# max_iter: how many iterations the algorithm should run for
# theta: tradeoff between exact O(n^2) and Barnes-Hut approximation O(n log n)
# eta: learning rate
# momentum: velocity of convergence
table_to_tsne <- function(table, dim = 2, per = 0, max_iter = 500, theta = 0.5,
                          eta = 200, momentum = 0.5, verbose = verbose_red)
{
  # the perplexity can't be too big
  stopifnot(per <= floor((nrow(table) - 1)/3))
  set.seed(0)

  Rtsne(table, dims = dim, perplexity = per, max_iter = max_iter, theta = theta,
        eta = eta, momentum = momentum, final_momentum = momentum, verbose = verbose,
        initial_dims = ncol(table), exaggeration_factor = 1, num_threads = 1,
        stop_lying_iter = 0, mom_switch_iter = 0,
        check_duplicates = FALSE, pca = FALSE, partial_pca = FALSE,
        is_distance = FALSE, Y_init = NULL,
        pca_center = FALSE, pca_scale = FALSE, normalize = FALSE)$Y
}

# -----------
# PCA METHODS
# -----------

# performs principal component analysis with no scaling on the provided table
table_to_pca <- function(table, dim = 2)
{
  sprintf_clean("PCA Table Dimensions: (%s, %s)", nrow(table), ncol(table))
  pca <- stats::prcomp(table, center = TRUE, rank. = dim)
  pca$rotation <- NULL
  pca$center <- NULL
  pca
}

# accessor helper function
pca_to_explore <- function(pca_result)
{
  pca_result$x
}

# generates a summary of variance captured vs number of components
pca_to_summary <- function(pca_result)
{
  eigs <- pca_result$sdev^2
  dim <- ncol(pca_result$x)
  props <- cumsum(eigs/sum(eigs))

  data.frame(
    "Components" = 1:dim,
    "Variance" = props[1:dim]
  )
}

# accessor helper function
pca_to_tsne <- function(pca_result, dim = 2, per = 0)
{
  table_to_tsne(pca_to_explore(pca_result), dim, per)
}

# -----------
# VAE METHODS
# -----------

# must be done immediately after loading Keras
tensorflow::use_condaenv("r-reticulate")
K <- keras::backend()

# disable eager execution
if (tensorflow::tf$executing_eagerly())
  tensorflow::tf$compat$v1$disable_eager_execution()

# ensure reproducibility
tensorflow::tf$random$set_seed(0)

# The reason we are using small layers
# is to improve scalability.
# Let A be the number of input neurons.
# Let d1 be the number of neurons in layer 1.
# Let d2 be the number of neurons in layer 2.
# Let B be the number of latent neurons.
get_intermediate <- function(input_dim, latent_dim)
{
  dim_d1 <- input_dim^0.5 * latent_dim^0.5
  dim_d2 <- dim_d1^0.5 * latent_dim^0.5
  dim_d1 <- min(input_dim, floor(dim_d1))
  dim_d2 <- min(input_dim, floor(dim_d2))
  c(dim_d1, dim_d2)
}

# stops the training after 'patience' epoches of nondecreasing val_loss
pat_callback <- function(patience, verbose = 1)
{
  callback_early_stopping(monitor = "val_loss", mode = "min", verbose = verbose,
                          patience = patience, restore_best_weights = TRUE)
}

# stops the training if the loss gradient explodes
naan_callback <- callback_terminate_on_naan()

# records the history of all iterations
loss <- numeric(0)
record_loss <- function(batch, logs){
  loss <<- c(loss, logs$loss)
}
record_callback <- callback_lambda(on_batch_end=record_loss)

# a fit function for a VAE
my_fit <- function(vae, x_train, batch_size = 2, patience = 10, max_epochs = 1000, verbose = 1){
  fit(
    vae,
    x = x_train,
    y = x_train,
    shuffle = TRUE,
    batch_size = batch_size,
    epochs = max_epochs,
    validation_split = 0.2,
    verbose = verbose,
    callbacks = list(pat_callback(patience, verbose), naan_callback, record_callback)
  )
}

# Variational Autoencoder
# batch_size: the number of samples per batch; lower takes more time but higher takes more memory
# patience: the number of epochs of nondecreasing loss that must occur before learning ends
# max_epochs: the maximum number of epochs that the VAE will run for (might stop earlier)
# verbose: 0 is silent, 1 is moderately verbose, 2 is highly verbose
table_to_vae <- function(table, dim = 2, batch_size = 2,
                         patience = 10, max_epochs = 1000, verbose = verbose_red)
{
  # require all entries to be within [0, 1]
  stopifnot(sum(table > 1) + sum(table < 0) == 0)

  num_samp <- nrow(table)
  input_dim <- ncol(table)
  latent_dim <- dim
  sprintf_clean("VAE Table Dimensions: (%s, %s)", num_samp, input_dim)

  dims <- get_intermediate(input_dim, latent_dim)
  dim_d1 <- dims[1]
  dim_d2 <- dims[2]
  sprintf_clean("Layers (x2): %s neurons, %s neurons", dim_d1, dim_d2)

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
  sampling <- function(arg){
    z_mean <- arg[, 1:latent_dim]
    z_log_var <- arg[, (latent_dim + 1):(2 * latent_dim)]

    epsilon <- k_random_normal(
      shape = c(k_shape(z_mean)[[1]]),
      mean = 0.0,
      stddev = 1.0
    )

    z_mean + k_exp(z_log_var/2)*epsilon
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
  vae_loss <- function(x, x_decoded_mean){
    xent_loss <- input_dim / 1.0 * loss_binary_crossentropy(x, x_decoded_mean)
    kl_loss <- k_mean(z_log_var - k_square(z_mean) - k_exp(z_log_var), axis = -1L)
    xent_loss + 1 - 0.5*kl_loss
  }

  # adam is faster and more accurate than rmsprop
  # amsgrad fixes a mathematical hole in the convergence
  # use gradient clipping to prevent an explosion
  # set learning rate low so the batch doesn't explode
  pref_comp <- optimizer_adam(learning_rate = 0.0001, amsgrad = TRUE, clipnorm = 0.1)

  vae %>% keras::compile(optimizer = pref_comp,
                         loss = vae_loss,
                         experimental_run_tf_function=FALSE)

  # reset loss before measurement
  loss <<- numeric()

  history <- my_fit(vae, table, batch_size, patience, max_epochs, ifelse(verbose, 2, 0))
  predict <- predict(encoder, table, batch_size)

  k_clear_session()

  list(
    "history" = history,
    "predict" = predict,
    "loss" = loss
  )
}

# accessor helper function
vae_to_explore <- function(vae_result)
{
  vae_result$predict
}

# extracts loss and val_loss from VAE results and makes a summary
vae_to_summary <- function(vae_result)
{
  loss <- vae_result$loss
  val_loss <- vae_result$history$metrics$val_loss
  num_losses <- length(loss)
  num_val_losses <- length(val_loss)

  # ensure you have even divisibility
  stopifnot(num_losses > 0, num_losses %% num_val_losses == 0)

  data.frame(
    "Training Iterations" = rep(1:num_losses, 2),
    "Loss Value" = c(loss, rep(val_loss, each = num_losses / num_val_losses)),
    "Loss Type" = rep(c("Testing Loss", "Validation Loss"), each = num_losses),
    check.names = FALSE
  )
}

# accessor helper function
vae_to_tsne <- function(vae_result, dim = 2, per = 0)
{
  table_to_tsne(vae_to_explore(vae_result), dim, per)
}

# ------------
# UMAP METHODS
# ------------

# Uniform Manifold Approximation and Projection
# note: UMAP perturbs duplicates to a degree - they will be close to each other
# but the duplicates will not have zero distance from each other!
table_to_umap <- function(table, dim = 2, per = 0, verbose = verbose_red)
{
  sprintf_clean("UMAP Table Dimensions: (%s, %s)", nrow(table), ncol(table))
  umap::umap(
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
}

# accessor helper function
umap_to_explore <- function(umap_result)
{
  umap_result$layout
}

# accessor helper function
umap_to_summary <- function(umap_result)
{
  umap_result$knn$indexes
}

# accessor helper function ... note that the same perplexity is used
umap_to_tsne <- function(umap_result, dim = 2)
{
  table_to_tsne(umap_to_explore(umap_result), dim, umap_result$config$n_neighbors)
}

# -------------
# PHATE METHODS
# -------------

# wrapper to make a matrix of normalized distribution samples
rnorm_mat <- function(row_n, col_n)
{
  matrix(rnorm(row_n * col_n), nrow = row_n, ncol = col_n)
}

# perturbs a matrix by a weighted normal distribution
perturb_mat <- function(mat, weight = 0.000000001)
{
  mat + weight * rnorm_mat(nrow(mat), ncol(mat))
}

# perturbs the duplicate rows of a matrix
perturb_duplicates <- function(mat, weight = 0.000000001)
{
  stopifnot(is.matrix(mat))
  dup_indices <- which(duplicated(mat))
  if (length(dup_indices) > 0)
    mat[dup_indices,] <- perturb_mat(mat[dup_indices,,drop = FALSE], weight)
  mat
}

# Potential of Heat diffusion for Affinity-based Transition Embedding
# note: PHATE by default does not work well with duplicate rows. The expression
# results$phate$embedding[result$phate$to_dup_indices,] restores the duplicates.
# note: returns the embedding and does not preserve other parts for storage reasons.
table_to_phate <- function(table, dim = 2, per = 0, verbose = verbose_red) {
  sprintf_clean("PHATE Table Dimensions: (%s, %s)", nrow(table), ncol(table))
  result <- phateR::phate(
    perturb_duplicates(table), ndim = dim, knn = per, decay = 40, n.landmark = 2000,
    gamma = 1, t = "auto", mds.solver = "sgd", knn.dist.method = "euclidean",
    init = NULL, mds.method = "metric", mds.dist.method = "euclidean",
    t.max = 100, npca = 10, verbose = verbose, n.jobs = 1, seed = 0)
  result$embedding
}

# ------------
# Sets METHODS
# ------------

# target[i, j] returns whether data[i, j] >= threshold,
# removing columns with no values at the threshold or above
# since all Sets undergo Global Min-Max, all values in table
# and the threshold must be in the range [0,1]
table_to_sets <- function(table, threshold) {
  # require all entries and the threshold to be within [0, 1]
  stopifnot(sum(table > 1) + sum(table < 0) == 0)
  stopifnot(between(threshold, 0, 1))

  sprintf_clean("Sets Table Dimensions: (%s, %s)", nrow(table), ncol(table))
  target <- matrix(as.numeric(table >= threshold), nrow=nrow(table), dimnames = dimnames(table))
  target[, colSums(target) > 0, drop = FALSE]
}

# given a binary matrix SETS from calculate_sets, let final[feature, label] be the
# fraction of samples with that label where that feature was present in SETS
set_label_matrix <- function(sets_result, labels){
  # validate that this is a binary matrix WITH FEATURE NAMES
  stopifnot(all.equal(class(matrix()), class(sets_result)))
  num_binary <- sum(sets_result == 0) + sum(sets_result == 1)
  stopifnot(num_binary == nrow(sets_result) * ncol(sets_result))
  stopifnot(length(colnames(sets_result)) == ncol(sets_result))

  # validate that labels is a vector of characters
  stopifnot(is.character(labels))

  rownames_final <- colnames(sets_result)
  summary <- summary(Matrix(sets_result, sparse = TRUE))
  summary_i <- summary[, 1]
  summary_j <- summary[, 2]

  set_types <- unique(labels)
  num_types <- length(set_types)
  rowlen_final <- length(rownames_final)

  final <- rep(0, rowlen_final*num_types)
  lookup <- match(labels, set_types)
  lookup2 <- (lookup - 1) * rowlen_final

  for (len in 1:length(summary_i))
  {
    num_i <- summary_i[len]
    index <- lookup2[num_i] + summary_j[len]
    final[index] <- final[index] + 1
  }

  final <- matrix(final, ncol = num_types)
  numbers <- tabulate(lookup, num_types)

  for (j in 1:num_types)
    final[,j] <- final[,j] / numbers[j]

  dimnames(final) <- list(rownames_final, set_types)
  final
}
