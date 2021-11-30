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

# ---------------
# GENERAL METHODS
# ---------------

# Parameters for methods:

# table_to_pca(table, dim = 2)
# - dim is the final number of dimensions

# table_to_vae(table, dim = 2, batch_size = 2, patience = 10, max_epochs = 1000)
# - dim is the final number of dimensions
# - batch_size is the number of samples per batch; lower takes more time but higher takes more memory
# - patience is the number of epochs of nondecreasing loss that must occur before learning ends
# - max_epochs is the maximum number of epochs that the VAE will run for (might stop earlier)

# determines if a table is valid for dimensionality reduction
# refuse to have less than 4 rows initially, since 3 points define a plane (2D)
# refuse to have less than 3 columns initially, since then you can immediately plot on 2D
valid_table <- function(cand_table)
{
  if (!all.equal(class(matrix()), class(cand_table)))
    return(FALSE)

  if (nrow(cand_table) < 4 || ncol(cand_table) < 3)
    return(FALSE)

    for (j in 1:ncol(cand_table))
      if (!is.numeric(cand_table[,j]))
        return(FALSE)

   TRUE
}

# -----------
# PCA METHODS
# -----------

table_to_pca <- function(table, dim = 2)
{
  pca <- stats::prcomp(table, center = TRUE, rank. = dim)
  pca$rotation <- NULL
  pca$center <- NULL
  pca
}

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

# -----------
# VAE METHODS
# -----------

# must be done immediately after loading Keras!!
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
pat_callback <- function(patience)
{
  callback_early_stopping(monitor = "val_loss", mode = "min", verbose = 1,
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
my_fit <- function(vae, x_train, batch_size = 2, patience = 10, max_epochs = 1000){
  fit(
    vae,
    x = x_train,
    y = x_train,
    shuffle = TRUE,
    batch_size = batch_size,
    epochs = max_epochs,
    validation_split = 0.2,
    verbose = 2,
    callbacks = list(pat_callback(patience), naan_callback, record_callback)
  )
}

# runs variational autoencoder
table_to_vae <- function(table, dim = 2, batch_size = 2, patience = 10, max_epochs = 1000)
{
  # require all entries to be within [0, 1]
  stopifnot(sum(table > 1) + sum(table < 0) == 0)

  num_samp <- nrow(table)
  input_dim <- ncol(table)
  latent_dim <- dim
  sprintf_clean("Table Dimensions: (%s, %s)", num_samp, input_dim)

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
  pref_comp <- optimizer_adam(lr = 0.0001, amsgrad = TRUE, clipnorm = 0.1)

  vae %>% keras::compile(optimizer = pref_comp,
                         loss = vae_loss,
                         experimental_run_tf_function=FALSE)

  # reset loss before measurement
  loss <<- numeric(0)

  history <- my_fit(vae, table, batch_size = batch_size, patience = patience, max_epochs = max_epochs)
  predict <- predict(encoder, table, batch_size = batch_size)

  k_clear_session()

  list(
    "history"=history,
    "predict"=predict,
    "loss"=loss
  )
}

# extracts loss and val_loss from VAE results and makes a summary
vae_to_summary <- function(vae_result)
{
  loss <- vae_result$loss
  val_loss <- vae_result$history$metrics$val_loss
  num_losses <- length(loss)

  data.frame(
    "Training Iterations" = rep(1:num_losses, 2),
    "Loss Value" = c(loss, rep(val_loss, each = num_losses / length(val_loss))),
    "Loss Type" = rep(c("Testing Loss", "Validation Loss"), each = num_losses),
    check.names = FALSE
  )
}

# ------------
# UMAP METHODS
# ------------

table_to_umap <- function(table, dim = 2, perp = 2)
{
  umap::umap(
    table,
    method = "naive",
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

umap_to_summary <- function(umap)
{
  umap$knn$indexes
}

# -------------
# PHATE METHODS
# -------------

table_to_phate <- function(data, dim = 2, perp = 1) {
  phateR::phate(
    data,
    ndim = dim,
    knn = perp,
    decay = 40,
    n.landmark = 2000,
    gamma = 1,
    t = "auto",
    mds.solver = "sgd",
    knn.dist.method = "euclidean",
    init = NULL,
    mds.method = "metric",
    mds.dist.method = "euclidean",
    t.max = 100,
    npca = 10,
    verbose = 1,
    n.jobs = 1,
    seed = 0)
}

# ------------
# tSNE METHODS
# ------------

# rTSNE with parameters set for speed on an m x n matrix
# dims (d): the number of final features; 1, 2, or 3
# perlexity (p): the perplexity (nearest neighbors) for tSNE
# max_iter (i): how many iterations the algorithm should run for
# theta (t): tradeoff between exact O(n^2) and Barnes-Hut approximation O(n log n)
# eta (e): learning rate
# momentum (u): velocity of convergence
# verbose (v): whether input is displayed as tSNE is run
# time complexity from tests: O(m^2 n i (d + p))
table_to_tsne <- function(table, dim = 2, perp = 1, max_iter = 500, theta = 0.5,
                     eta = 200, momentum = 0.5, verbose = FALSE)
{
  perplexity <- min(perp, floor((nrow(table)-1)/3))
  set.seed(42)

  Rtsne(table, dims = dim, perplexity = perp, max_iter = max_iter, theta = theta,
        eta = eta, momentum = momentum, final_momentum = momentum, verbose = verbose,
        initial_dims = ncol(table), exaggeration_factor = 1, num_threads = 1,
        stop_lying_iter = 0, mom_switch_iter = 0,
        check_duplicates = FALSE, pca = FALSE, partial_pca = FALSE,
        is_distance = FALSE, Y_init = NULL,
        pca_center = FALSE, pca_scale = FALSE, normalize = FALSE)
}
