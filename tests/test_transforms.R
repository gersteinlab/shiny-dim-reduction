# This file tests transforms.R.
# source("tests/test_transforms.R")

# -----
# SETUP
# -----

source("pipeline/transforms.R")

# scale options
sca_options <- c("Logarithmic", "Linear")
# normalization options
nor_options <- c("Global Min-Max", "Local Min-Max",
                 "Global Z-Score", "Local Z-Score",
                 "Quantile")

# -----
# TESTS
# -----

test_sca_nor <- function(mat)
{
  results <- empty_named_list(sca_options)

  for (sca in sca_options)
  {
    results[[sca]] <- empty_named_list(nor_options)
    sca_mat <- do_scal(sca, mat)

    for (nor in nor_options)
    {
      data <- do_norm(nor, sca_mat)
      results[[sca]][[nor]] <- data
      cat_f("For %s Scaling and %s Normalization: Mean = %0.3f, SD = %0.3f\n",
                    sca, nor, mean(data), sd(data))
    }
  }

  results
}

test_matrix <- matrix(1:200, nrow = 20)
test_result <- test_sca_nor(test_matrix)
