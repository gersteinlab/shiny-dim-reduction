# The goal of this script is to perform dimensionality reduction with VAE.

setwd(sprintf("%s/shiny-dim-reduction", Sys.getenv("SHINY_DIM_REDUCTION_ROOT")))
source("scaling.R", encoding="UTF-8")

library(reticulate)
library(tfruns)
library(tensorflow)
library(keras)

# --------------
# USER VARIABLES
# --------------

# latent dimensions ... the smallest neuron layer in VAE
latent_dim <- pc_cap
# batch_size depends completely on the dataset and your willingness to wait
batch_size <- 2
# cap_size depends completely on the dataset and your willingness to wait
cap_size <- 12000
# patience depends completely on the dataset and your willingness to wait (10-20 standard)
pat_size <- 5

# ---------
# FUNCTIONS
# ---------

# must be done immediately after loading Keras!!
K <- keras::backend()
tensorflow::tf$random$set_seed(0)

# caps the data at cap_size features
cap <- function(data)
{
  data[,1:min(ncol(data), cap_size)]
}

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

# loss function
vae_loss_full <- function(x, x_decoded_mean, dim, z_mean, z_log_var){
  xent_loss <- dim/1.0*loss_binary_crossentropy(x, x_decoded_mean)
  kl_loss <- k_mean(z_log_var - k_square(z_mean) - k_exp(z_log_var), axis = -1L)
  xent_loss + 1 - 0.5*kl_loss
}

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

# stops the training after no further progress is made
early <- callback_early_stopping(
  monitor='val_loss', mode='min', 
  verbose=1, patience=pat_size,
  restore_best_weights = TRUE)

# stops the training if the loss gradient explodes
naani <- callback_terminate_on_naan()

# records the history of all iterations
loss <- numeric(0)
loss_rec <- function(batch, logs){
  loss <<- c(loss, logs$loss)
}
histo <- callback_lambda(on_batch_end=loss_rec)

# -----------------------
# VARIATIONAL AUTOENCODER
# -----------------------

# adam is faster and more accurate than rmsprop
# amsgrad fixes a mathematical hole in the convergence
# use gradient clipping to prevent an explosion
# set learning rate low so the batch doesn't explode
pref_comp <- optimizer_adam(
  lr = 0.0001,
  amsgrad = TRUE,
  clipnorm = 0.1)

# a fit function for a VAE
my_fit <- function(vae, x_train){
  fit(
    vae, 
    x = x_train, 
    y = x_train,
    shuffle = TRUE,
    batch_size = batch_size,
    epochs = 1000,
    validation_split = 0.2,
    verbose = 2,
    callbacks=list(early, naani, histo)
  )
}

# making records from loss, val_loss
rec_colnames <- c("loss", "val_loss")
make_records <- function(loss, val_loss)
{
  batch_per_epoch <- length(loss) / length(val_loss)
  records <- matrix(0, nrow=length(loss), ncol=2)
  colnames(records) <- rec_colnames
  records[,1] <- loss
  for (i in 1:length(val_loss))
  {
    range <- (i-1)*batch_per_epoch + 1:batch_per_epoch
    records[range, 2] <- rep(val_loss[i], batch_per_epoch)
  }
  records
}

dog <- rev(names(categories))
for (cat in dog)
{
  combined <- readRDS(sprintf("combined/combined_%s.rds", cat))
  
  for (sub in sub_groups[[cat]])
  {
    scaled <- get_safe_sub(sub, combined, decorations, cat)
    
    for (sca in sca_options)
    {
      scaled <- do_scal(sca, scaled)
      
      for (nor in nor_options[1:2])
      {
        scaled <- do_norm(nor, scaled)
        
        for (fea in c(1, 10, 100))
        {
          vae_title <- sprintf("VAE/VAE_%s_%s_%s_%s_%s.rds", fea, nor, sca, sub, cat)
          if (!file.exists(vae_title))
          {
            print(vae_title)
            data <- feature_start(cap(scaled), fea/100)
            
            num_samp <- nrow(data)
            input_dim <- ncol(data)
            print(sprintf("Number of features: %s", input_dim))
            
            dims <- get_intermediate(input_dim, latent_dim)
            dim_d1 <- dims[1]
            dim_d2 <- dims[2]
            print(sprintf("Layers (x2): %s neurons, %s neurons", dim_d1, dim_d2))
            
            x_inputs <- layer_input(shape = input_dim)
            enc_d1 <- layer_dense(x_inputs, dim_d1, activation = "relu")
            enc_d2 <- layer_dense(enc_d1, dim_d2, activation = "relu")
            
            z_dec_d2 <- layer_dense(units = dim_d2, activation = "relu")
            d2_dec_d1 <- layer_dense(units = dim_d1, activation = "relu")
            d1_dec_means <- layer_dense(units = input_dim, activation = "sigmoid")
            
            z_mean <- layer_dense(enc_d2, latent_dim)
            z_log_var <- layer_dense(enc_d2, latent_dim)
            z_combine <- layer_concatenate(list(z_mean, z_log_var))
            
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
              vae_loss_full(x, x_decoded_mean, input_dim, z_mean, z_log_var)
            }
            
            vae %>% keras::compile(optimizer = pref_comp,
                                   loss = vae_loss,
                                   experimental_run_tf_function=FALSE)
            
            # generate training data
            x_train <- data[,1:input_dim]
            
            loss <- numeric(0)
            
            history <- my_fit(vae, x_train)
            
            vae_final <- list(
              "history"=history,
              "predict"=predict(encoder, data[,1:input_dim], batch_size = batch_size),
              "records"=make_records(loss, history$metrics$val_loss)
            )
            
            saveRDS(vae_final, vae_title)
            
            k_clear_session()
          }
        }
      }
    }
  }
}