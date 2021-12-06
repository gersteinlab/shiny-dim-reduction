# This file tests perform_reduction.R.

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

source_sdr("perform_reduction.R")

# a collection of invalid potential inputs
invalid_candidates <- c(
  NA
  # , "~!@#$%^&*()", .Machine$double.xmin, -.Machine$double.xmax, NaN
)

# used to populate invalid inputs randomly
inv <- function()
{
  sample(invalid_candidates, 1)
}

# -------------------
# TEST VALID REQUESTS
# -------------------

print_clean("This is what a default request looks like: ")
print(make_requests())

valid1 <- make_requests(
  "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "PCA", "Explore",
  10, inv(), inv(), inv(), inv()
)
valid2 <- make_requests(
  "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "PCA", "Summarize",
  10, inv(), inv(), inv(), inv()
)
valid3 <- make_requests(
  "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "PCA", "tSNE",
  10, 2, 25, inv(), inv()
)
valid4 <- make_requests(
  "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "VAE", "Explore",
  10, inv(), inv(), 64, inv()
)
valid5 <- make_requests(
  "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "VAE", "Summarize",
  10, inv(), inv(), 64, inv()
)
valid6 <- make_requests(
  "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "VAE", "tSNE",
  10, 2, 15, 30, inv()
)
valid7 <- make_requests(
  "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "UMAP", "Explore",
  10, inv(), 25, inv(), inv()
)
valid8 <- make_requests(
  "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "UMAP", "Summarize",
  10, inv(), 25, inv(), inv()
)
valid9 <- make_requests(
  "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "UMAP", "tSNE",
  10, 2, 25, inv(), inv()
)

valid10 <- make_requests(
  "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "PHATE", inv(),
  2, inv(), 25, inv(), inv()
)

valid11 <- make_requests(
  "miRNA", "Plasma", "SD_Top_100", "Linear", "Quantile", "PHATE", inv(),
  2, inv(), 25, inv(), inv()
)

valid12 <- make_requests(
  "miRNA", inv(), inv(), "Logarithmic", "Global Min-Max", "Sets", inv(),
  inv(), inv(), inv(), inv(), 0.4
)

val_req <- rbind(
  valid1, valid2, valid3, valid4, valid5, valid6,
  valid7, valid8, valid9, valid10, valid11, valid12
)

sprintf_clean("Number of Valid Requests: Expected 12, Received %s", nrow(val_req))

# ----------------------
# PERFORM VALID REQUESTS
# ----------------------

# ---------------------
# TEST INVALID REQUESTS
# ---------------------

# attributes of wrong length
invalid1 <- make_requests(inv())

# batch size wrong
invalid2 <- make_requests(
  "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "VAE", "tSNE",
  10, 2, 15, "dog", "hi"
)

# perplexity too large
invalid3 <- make_requests(
  "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "VAE", "tSNE",
  10, 2, 5000, 30, inv()
)

# Sets with wrong normalization
invalid4 <- make_requests(
    "miRNA", inv(), inv(), "Logarithmic", "Quantile", "Sets", inv(),
    inv(), inv(), inv(), inv(), 0.4
)

# VAE with wrong normalization
invalid5 <- make_requests(
  "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Quantile", "VAE", "tSNE",
  10, 2, 15, 64, "hi"
)

# PHATE but no perplexity provided
invalid6 <- make_requests(
  "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "PHATE", inv(),
  2, inv(), inv(), inv(), inv()
)

sprintf_clean("Is expected invalid request 1 valid?: %s", !is.null(invalid1))
sprintf_clean("Is expected invalid request 2 valid?: %s", !is.null(invalid2))
sprintf_clean("Is expected invalid request 3 valid?: %s", !is.null(invalid3))
sprintf_clean("Is expected invalid request 4 valid?: %s", !is.null(invalid4))
sprintf_clean("Is expected invalid request 5 valid?: %s", !is.null(invalid5))
sprintf_clean("Is expected invalid request 6 valid?: %s", !is.null(invalid6))

# table_name <- paste(test_requests[4, 1:5], collapse = "_")
# pca_100_plasma <- readRDS(sprintf("inter/%s_%s_%s.rds", table_name, "PCA", 10))
# subset_labels <- order_total$miRNA[get_row_decor_indices("miRNA", "Plasma"),,drop=FALSE]
# plotly_2d(pca_100_plasma$x[,1], pca_100_plasma$x[,2], subset_labels$BIOFLUID)
# plotly_2d(pca_100_plasma$x[,1], pca_100_plasma$x[,2], subset_labels$CONDITION)
