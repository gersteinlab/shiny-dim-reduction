# This file tests make_requests.R.
# source("tests/test_make_requests.R")

# -----
# SETUP
# -----

library(testthat)

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


valid1 <- make_req_keys(
  "miRNA", "Total", "Total", "Logarithmic", "Global Min-Max", "PCA", "Explore",
  10L, -1L, -1L, -1L, pi, "!"
) %>% make_requests("ADMIN")

valid2 <- make_req_keys(
  "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "PCA", "Summarize",
  10L, -1L, -1L, -1L, pi, "!"
) %>% make_requests("ADMIN")

valid3 <- make_req_keys(
  "miRNA", "Total", "SD_Top_100", "Logarithmic", "Global Min-Max", "PCA", "tSNE",
  10L, 2L, 25L, -1L, pi, "!"
) %>% make_requests("ADMIN")

valid4 <- make_req_keys(
  "miRNA", "Urine", "SD_Top_100", "Logarithmic", "Global Min-Max", "VAE", "Explore",
  10L, -1L, -1L, 64L, pi, "!"
) %>% make_requests("ADMIN")

valid5 <- make_req_keys(
  "miRNA", "Urine", "SD_Top_100", "Logarithmic", "Global Min-Max", "VAE", "Summarize",
  10L, -1L, -1L, 64L, pi, "!"
) %>% make_requests("ADMIN")

valid6 <- make_req_keys(
  "miRNA", "Urine", "SD_Top_100", "Logarithmic", "Global Min-Max", "VAE", "tSNE",
  10L, 2L, 15L, 30L, pi, "!"
) %>% make_requests("ADMIN")

valid7 <- make_req_keys(
  "miRNA", "Plasma", "SD_Top_100", "Logarithmic", "Global Min-Max", "UMAP", "Explore",
  10L, -1L, 25L, -1L, pi, "!"
) %>% make_requests("ADMIN")

valid8 <- make_req_keys(
  "miRNA", "Plasma", "SD_Top_100", "Logarithmic", "Global Min-Max", "UMAP", "Summarize",
  10L, -1L, 25L, -1L, pi, "!"
) %>% make_requests("ADMIN")

valid9 <- make_req_keys(
  "miRNA", "Plasma", "SD_Top_100", "Logarithmic", "Global Min-Max", "UMAP", "tSNE",
  10L, 2L, 25L, -1L, pi, "!"
) %>% make_requests("ADMIN")

valid10 <- make_req_keys(
  "miRNA", "Saliva", "SD_Top_100", "Logarithmic", "Global Min-Max", "PHATE", "!",
  2L, -1L, 25L, -1L, pi, "!"
) %>% make_requests("ADMIN")

valid11 <- make_req_keys(
  "miRNA", "Saliva", "SD_Top_100", "Linear", "Quantile", "PHATE", "!",
  2L, -1L, 25L, -1L, pi, "!"
) %>% make_requests("ADMIN")

valid12 <- make_req_keys(
  "miRNA", "!", "!", "Logarithmic", "Global Min-Max", "Sets", "!",
  -1L, -1L, -1L, -1L, 0.4, "CONDITION"
) %>% make_requests("ADMIN")

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
  make_req_keys(
    "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "PCA", "Explore",
    10L, -1L, -1L, -1L, pi, "!"
  ) %>% make_requests(character()) %>% expect_error()

  # invalid type
  make_req_keys(
    "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "PCA", "Explore",
    10L, -1L, -1L, pi, pi, "!"
  ) %>% make_requests("ADMIN") %>% expect_error()

  # nonpositive batch size
  make_req_keys(
    "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "VAE", "tSNE",
    10L, 2L, 15L, bat = 0L, num_d, chr_d
  ) %>% make_requests("ADMIN") %>% expect_error()

  # perplexity too large
  make_req_keys(
    "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "VAE", "tSNE",
    10L, 2L, per = 5000L, 30L, num_d, chr_d
  ) %>% make_requests("ADMIN") %>% expect_error()

  # invalid normalization for Sets
  make_req_keys(
    "miRNA", chr_d, chr_d, "Logarithmic", nor = "Quantile", "Sets", chr_d,
    int_d, int_d, int_d, int_d, 0.4, chr_d
  ) %>% make_requests("ADMIN") %>% expect_error()

  # invalid normalization for VAE
  make_req_keys(
    "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Quantile", "VAE", "tSNE",
    10L, 2L, 15L, 64L, num_d, chr_d
  ) %>% make_requests("ADMIN") %>% expect_error()

  # PHATE but invalid perplexity
  make_req_keys(
    "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "PHATE", chr_d,
    2L, int_d, per = 0L, int_d, num_d, chr_d
  ) %>% make_requests("ADMIN") %>% expect_error()
})

# --------------------
# TEST REQ_KEY HELPERS
# --------------------

test_that("req_key helpers work", {
  make_pca_e_req_keys(
    "miRNA", "Total", "Total", "Linear",
    "Global Min-Max", com = 10L
  ) %>% clean_req_keys() %>%
    are_req_keys() %>% expect_true()

  make_pca_s_req_keys(
    "miRNA", "Total", "Total", "Linear",
    "Global Min-Max", com = 10L
  ) %>% clean_req_keys() %>%
    are_req_keys() %>% expect_true()

  make_vae_e_req_keys(
    "miRNA", "Total", "Total", "Linear",
    "Global Min-Max", com = 10L, bat = 10L
  ) %>% clean_req_keys() %>%
    are_req_keys() %>% expect_true()

  make_vae_s_req_keys(
    "miRNA", "Total", "Total", "Linear",
    "Global Min-Max", com = 10L, bat = 10L
  ) %>% clean_req_keys() %>%
    are_req_keys() %>% expect_true()

  make_umap_e_req_keys(
    "miRNA", "Total", "Total", "Linear",
    "Global Min-Max", com = 10L, per = 10L
  ) %>% clean_req_keys() %>%
    are_req_keys() %>% expect_true()

  make_umap_s_req_keys(
    "miRNA", "Total", "Total", "Linear",
    "Global Min-Max", com = 10L, per = 10L
  ) %>% clean_req_keys() %>%
    are_req_keys() %>% expect_true()
})

# ---------------
# TEST CLEAN TIME
# ---------------

source_app("storage.R")
set_store_mode("local")
load_all_stores()
app_requests <- load_store("app_requests.rds")
print(system.time({
  test1 <- clean_req_keys(app_requests)
}))
print(system.time({
  test2 <- name_req_key_files(app_requests[, 1:13])
}))

# -------
# CLEANUP
# -------

message_f("TESTING TIME (seconds): %.1f", time_diff(start_time))
