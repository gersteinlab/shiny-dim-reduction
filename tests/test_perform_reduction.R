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

# optional but default for now; avoids prompt
project_name <- "exRNA"
source_sdr("perform_reduction.R")

# -------------------
# TEST VALID REQUESTS
# -------------------

print_clean("This is what a default request looks like: ")
print(make_requests())

valid1 <- make_requests(
  "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "PCA", "Explore",
  10, -1, -1, -1, pi, "!", aut_d()
)
valid2 <- make_requests(
  "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "PCA", "Summarize",
  10, -1, -1, -1, pi, "!", aut_d()
)
valid3 <- make_requests(
  "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "PCA", "tSNE",
  10, 2, 25, -1, pi, "!", aut_d()
)
valid4 <- make_requests(
  "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "VAE", "Explore",
  10, -1, -1, 64, pi, "!", aut_d()
)
valid5 <- make_requests(
  "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "VAE", "Summarize",
  10, -1, -1, 64, pi, "!", aut_d()
)
valid6 <- make_requests(
  "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "VAE", "tSNE",
  10, 2, 15, 30, pi, "!", aut_d()
)
valid7 <- make_requests(
  "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "UMAP", "Explore",
  10, -1, 25, -1, pi, "!", aut_d()
)
valid8 <- make_requests(
  "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "UMAP", "Summarize",
  10, -1, 25, -1, pi, "!", aut_d()
)
valid9 <- make_requests(
  "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "UMAP", "tSNE",
  10, 2, 25, -1, pi, "!", aut_d()
)

valid10 <- make_requests(
  "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "PHATE", "!",
  2, -1, 25, -1, pi, "!", aut_d()
)

valid11 <- make_requests(
  "miRNA", "Plasma", "SD_Top_100", "Linear", "Quantile", "PHATE", "!",
  2, -1, 25, -1, pi, "!", aut_d()
)

valid12 <- make_requests(
  "miRNA", "!", "!", "Logarithmic", "Global Min-Max", "Sets", "!",
  -1, -1, -1, -1, 0.4, "CONDITION", aut_d()
)

val_req <- rbind(
  valid1, valid2, valid3, valid4, valid5, valid6,
  valid7, valid8, valid9, valid10, valid11, valid12
)

sprintf_clean("Number of Valid Requests: Expected 12, Received %s", nrow(val_req))

# ---------------------
# TEST INVALID REQUESTS
# ---------------------

# wrong length
invalid1 <- make_requests(
  "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "PCA", "Explore",
  10, -1, -1, -1, pi, "!", character(0)
)
# wrong type
invalid2 <- make_requests(
  "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "PCA", "Explore",
  10, -1, -1, pi, pi, "!", aut_d()
)
# batch size too small
invalid3 <- make_requests(
  "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "VAE", "tSNE",
  10, 2, 15, -1, num_d(), chr_d(), aut_d()
)
# perplexity too large
invalid4 <- make_requests(
  "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "VAE", "tSNE",
  10, 2, 5000, 30, num_d(), chr_d(), aut_d()
)
# Sets with wrong normalization
invalid5 <- make_requests(
    "miRNA", chr_d(), chr_d(), "Logarithmic", "Quantile", "Sets", chr_d(),
    num_d(), num_d(), num_d(), num_d(), 0.4, chr_d(), aut_d()
)
# VAE with wrong normalization
invalid6 <- make_requests(
  "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Quantile", "VAE", "tSNE",
  10, 2, 15, 64, num_d(), chr_d(), aut_d()
)
# PHATE but negative perplexity
invalid7 <- make_requests(
  "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "PHATE", chr_d(),
  2, num_d(), -10, num_d(), num_d(), chr_d(), aut_d()
)

invalid_requests <- list(
  invalid1, invalid2, invalid3, invalid4, invalid5, invalid6, invalid7
)

for (i in seq_along(invalid_requests))
  sprintf_clean("Is expected invalid request %s valid?: %s", i, !is.null(invalid_requests[[i]]))

# ----------------------
# PERFORM VALID REQUESTS
# ----------------------

val_res <- perform_reduction(val_req, 0)
# val_res <- perform_reduction(val_req, 1)
# val_res <- perform_reduction(val_req, 2)
