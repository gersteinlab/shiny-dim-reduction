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


test_table <- matrix(0, nrow = 1000, ncol = 20)

for (i in 1:nrow(test_table))
  for (j in 1:ncol(test_table))
    test_table[i,j] <- sample(1:100/100, 1)

sprintf_clean("Is this a valid table?: %s", valid_table(test_table))

lol <- table_to_umap(test_table, 10, 20)

if (tensorflow::tf$executing_eagerly())
  tensorflow::tf$compat$v1$disable_eager_execution()

derp <- table_to_vae(test_table)
d2 <- table_to_phate(test_table, 10, 20)

conda install -c conda-forge umap-learn==0.4
conda install keras matplotlib numba pandas scikit-learn
pip install phate

# reticulate::use_condaenv("r-reticulate", conda = "C:/Anaconda/Scripts/conda.exe")
# keras::use_condaenv("r-reticulate", conda = "C:/Anaconda/Scripts/conda.exe")
# keras::install_keras(method = "conda", conda = "C:/Anaconda/Scripts/conda.exe")
# Sys.setenv("RETICULATE_PYTHON" = "C:\\Anaconda\\envs\\r-reticulate\\python.exe")
# library(reticulate)
# library(tensorflow)
# tensorflow::use_condaenv("r-reticulate")
#
# install_tensorflow(method = 'conda', envname = 'r-reticulate')
#
# reticulate::py_discover_config()
# reticulate::py_config()
# library(tensorflow)
