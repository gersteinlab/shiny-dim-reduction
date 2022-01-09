# This file tests sca_nor_fun.R.

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

source_sdr("sca_nor_fun.R")

# scale options
sca_options <- c("Logarithmic", "Linear")
# normalization options
nor_options <- c("Global Min-Max", "Local Min-Max",
                 "Global Z-Score", "Local Z-Score",
                 "Quantile")

test_sca_nor <- function(mat)
{
  results <- empty_named_list(sca_options)

  for (sca in sca_options)
  {
    results[[sca]] <- empty_named_list(nor_options)
    sca_mat <- do_scal(sca, mat)

    for (nor in nor_options)
    {
      results[[sca]][[nor]] <- do_norm(nor, sca_mat)
      sprintf_clean("For %s Scaling and %s Normalization: Mean = %0.3f, SD = %0.3f",
                    sca, nor, mean(results[[sca]][[nor]]), sd(results[[sca]][[nor]]))
    }
  }

  results
}

# -----
# TESTS
# -----

test_matrix <- matrix(1:200, nrow = 20)
test_result <- test_sca_nor(test_matrix)
