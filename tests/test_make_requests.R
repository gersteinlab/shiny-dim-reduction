# This file tests make_requests.R.
# source("tests/test_make_requests.R")

# -----
# SETUP
# -----

source("app/install.R")
if (!require(testthat))
  stop("Missing package: testthat")

source("app/make_requests.R")
start_time <- Sys.time()

# -------------------
# TEST VALID REQUESTS
# -------------------

test_that("make_requests() works for default input", {
  default_requests <- make_requests()

  cat("This is what a default request looks like:\n")
  print(default_requests)

  are_requests(default_requests) %>% expect_true()
})


valid1 <- make_requests(
  "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "PCA", "Explore",
  10L, -1L, -1L, -1L, pi, "!", "ADMIN"
)

valid2 <- make_requests(
  "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "PCA", "Summarize",
  10L, -1L, -1L, -1L, pi, "!", "ADMIN"
)

valid3 <- make_requests(
  "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "PCA", "tSNE",
  10L, 2L, 25L, -1L, pi, "!", "ADMIN"
)

valid4 <- make_requests(
  "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "VAE", "Explore",
  10L, -1L, -1L, 64L, pi, "!", "ADMIN"
)

valid5 <- make_requests(
  "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "VAE", "Summarize",
  10L, -1L, -1L, 64L, pi, "!", "ADMIN"
)

valid6 <- make_requests(
  "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "VAE", "tSNE",
  10L, 2L, 15L, 30L, pi, "!", "ADMIN"
)

valid7 <- make_requests(
  "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "UMAP", "Explore",
  10L, -1L, 25L, -1L, pi, "!", "ADMIN"
)

valid8 <- make_requests(
  "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "UMAP", "Summarize",
  10L, -1L, 25L, -1L, pi, "!", "ADMIN"
)

valid9 <- make_requests(
  "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "UMAP", "tSNE",
  10L, 2L, 25L, -1L, pi, "!", "ADMIN"
)

valid10 <- make_requests(
  "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "PHATE", "!",
  2L, -1L, 25L, -1L, pi, "!", "ADMIN"
)

valid11 <- make_requests(
  "miRNA", "Plasma", "SD_Top_100", "Linear", "Quantile", "PHATE", "!",
  2L, -1L, 25L, -1L, pi, "!", "ADMIN"
)

valid12 <- make_requests(
  "miRNA", "!", "!", "Logarithmic", "Global Min-Max", "Sets", "!",
  -1L, -1L, -1L, -1L, 0.4, "CONDITION", "ADMIN"
)

val_req <- rbind_req(
  valid1, valid2, valid3, valid4, valid5, valid6,
  valid7, valid8, valid9, valid10, valid11, valid12
)

test_that("make_requests() works for valid input", {
  expect_equal(nrow(val_req), 12)
})

# ---------------------
# TEST INVALID REQUESTS
# ---------------------

test_that("make_requests() fails for invalid input", {
  # wrong length
  make_requests(
    "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "PCA", "Explore",
    10L, -1L, -1L, -1L, pi, "!", character(0)
  ) %>% expect_error()

  # invalid type
  make_requests(
    "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "PCA", "Explore",
    10L, -1L, -1L, pi, pi, "!", "ADMIN"
  ) %>% expect_error()

  # nonpositive batch size
  make_requests(
    "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "VAE", "tSNE",
    10L, 2L, 15L, bat = 0L, num_d, chr_d, "ADMIN"
  ) %>% expect_error()

  # perplexity too large
  make_requests(
    "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "VAE", "tSNE",
    10L, 2L, per = 5000L, 30L, num_d, chr_d, "ADMIN"
  ) %>% expect_error()

  # invalid normalization for Sets
  make_requests(
    "miRNA", chr_d, chr_d, "Logarithmic", nor = "Quantile", "Sets", chr_d,
    int_d, int_d, int_d, int_d, 0.4, chr_d, "ADMIN"
  ) %>% expect_error()

  # invalid normalization for VAE
  make_requests(
    "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Quantile", "VAE", "tSNE",
    10L, 2L, 15L, 64L, num_d, chr_d, "ADMIN"
  ) %>% expect_error()

  # PHATE but invalid perplexity
  invalid7 <- make_requests(
    "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "PHATE", chr_d,
    2L, int_d, per = 0L, int_d, num_d, chr_d, "ADMIN"
  ) %>% expect_error()
})

# ---------------
# TEST CLEAN TIME
# ---------------

print(system.time({
  clean_req_keys(app_requests)
}))

# -------
# CLEANUP
# -------

message_f("TESTING TIME (seconds): %.1f", time_diff(start_time))
