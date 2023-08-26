# The purpose of this file is to give examples for request management.

if (!exists("sdr_config") || sdr_config$mode != "pipeline")
  source("app/install.R")
stopifnot(sdr_config$mode == "pipeline")

source("pipeline/red_requests.R")
set_workflow("MNIST")

# ------------------
# REQ_KEY GENERATION
# ------------------

# simplifying perplexity for now
perplexity_types <- 20L

# searches for a threshold to num_digits precision such that
# ncol(table_to_sets(data, thre)) approximates target
binary_search <- function(data, target, num_digits)
{
  precision <- 0.1^num_digits
  lower <- precision
  upper <- 1
  while (upper - lower >= precision)
  {
    mid <- (lower + upper)/2

    if (sum(colSums(data >= mid) > 0) < target)
      upper <- mid
    else
      lower <- mid
  }

  round((lower + upper)/2, num_digits)
}

# ------------
# PCA REQUESTS
# ------------

pca_req_list <- list()
pca_req_append <- function(item)
{
  pca_req_list[[length(pca_req_list) + 1]] <<- item
}

for (cat in cat_names)
{
  cat_row_sub_lengths <- get_row_sub_lengths(cat)
  for (row in names(cat_row_sub_lengths))
  {
    cat_col_sub_lengths <- get_col_sub_lengths(cat)
    for (col in names(cat_col_sub_lengths))
    {
      for (sca in sca_options)
      {
        for (nor in nor_options_by_emb("PCA"))
        {
          # explore
          pca_e <- make_pca_e_req_keys(
            cat, row, col, sca, nor, com = 10L)
          pca_req_append(pca_e)

          # summarize
          pca_s <- make_pca_s_req_keys(
            cat, row, col, sca, nor, com = 10L)
          pca_req_append(pca_s)

          # tsne
          for (per in perplexity_types)
          {
            if (perplexity_is_valid(per, cat_row_sub_lengths[[row]]))
            {
              for (dim in c(2L, 3L))
              {
                pca_t <- make_pca_req_keys(
                  cat, row, col, sca, nor,
                  vis = "tSNE", com = 10L, dim, per)
                pca_req_append(pca_t)
              }
            }
          }
        }
      }
    }
  }
}

pca_req_keys <- do.call(rbind, pca_req_list)
pca_requests <- make_requests(pca_req_keys, "ADMIN")

# ------------
# VAE REQUESTS
# ------------

vae_req_list <- list()
vae_req_append <- function(item)
{
  vae_req_list[[length(vae_req_list) + 1]] <<- item
}

for (cat in cat_names)
{
  cat_row_sub_lengths <- get_row_sub_lengths(cat)
  for (row in names(cat_row_sub_lengths))
  {
    cat_col_sub_lengths <- get_col_sub_lengths(cat)
    for (col in names(cat_col_sub_lengths))
    {
      for (sca in sca_options)
      {
        for (nor in nor_options_by_emb("VAE"))
        {
          # explore
          vae_e <- make_vae_e_req_keys(
            cat, row, col, sca, nor, com = 10L, bat = 64L)
          vae_req_append(vae_e)

          # summarize
          vae_s <- make_vae_s_req_keys(
            cat, row, col, sca, nor, com = 10L, bat = 64L)
          vae_req_append(vae_s)

          # tsne
          for (per in perplexity_types)
          {
            for (dim in c(2L, 3L))
            {
              vae_t <- make_vae_req_keys(
                cat, row, col, sca, nor, vis = "tSNE",
                com = 10L, dim, per, bat = 64L)
              vae_req_append(vae_t)
            }
          }
        }
      }
    }
  }
}

vae_req_keys <- do.call(rbind, vae_req_list)
vae_requests <- make_requests(vae_req_keys, "ADMIN")

# -------------
# UMAP REQUESTS
# -------------

umap_req_list <- list()
umap_req_append <- function(item)
{
  umap_req_list[[length(umap_req_list) + 1]] <<- item
}

for (cat in cat_names)
{
  cat_row_sub_lengths <- get_row_sub_lengths(cat)
  for (row in names(cat_row_sub_lengths))
  {
    cat_col_sub_lengths <- get_col_sub_lengths(cat)
    for (col in names(cat_col_sub_lengths))
    {
      for (sca in sca_options)
      {
        for (nor in nor_options_by_emb("UMAP"))
        {
          for (per in perplexity_types)
          {
            # explore
            umap_e <- make_umap_e_req_keys(
              cat, row, col, sca, nor, com = 10L, per)
            umap_req_append(umap_e)

            # summarize
            umap_s <- make_umap_s_req_keys(
              cat, row, col, sca, nor, com = 10L, per)
            umap_req_append(umap_s)

            # tsne
            for (dim in c(2L, 3L))
            {
              umap_t <- make_umap_req_keys(
                cat, row, col, sca, nor, vis = "tSNE",
                com = 10L, dim, per)
              umap_req_append(umap_t)
            }
          }
        }
      }
    }
  }
}

umap_req_keys <- do.call(rbind, umap_req_list)
umap_requests <- make_requests(umap_req_keys, "ADMIN")

# --------------
# PHATE REQUESTS
# --------------

phate_req_list <- list()
phate_req_append <- function(item)
{
  phate_req_list[[length(phate_req_list) + 1]] <<- item
}

for (cat in cat_names)
{
  cat_row_sub_lengths <- get_row_sub_lengths(cat)
  for (row in names(cat_row_sub_lengths))
  {
    cat_col_sub_lengths <- get_col_sub_lengths(cat)
    for (col in names(cat_col_sub_lengths))
    {
      for (sca in sca_options)
      {
        for (nor in nor_options_by_emb("PHATE"))
        {
          for (per in perplexity_types)
          {
            for (com in c(2L, 3L))
            {
              make_phate_req_keys(
                cat, row, col, sca, nor, com, per
              ) %>% phate_req_append()
            }
          }
        }
      }
    }
  }
}

phate_req_keys <- do.call(rbind, phate_req_list)
phate_requests <- make_requests(phate_req_keys, "ADMIN")

# -------------
# SETS REQUESTS
# -------------

lower <- 8
upper <- 784
len_inter <- 10
num_digits <- 5

sets_req_list <- list()
sets_req_append <- function(item)
{
  sets_req_list[[length(sets_req_list) + 1]] <<- item
}

for (cat in cat_names)
{
  short_list <- get_row_axis(cat)$rel_meta
  combined <- readRDS(get_cat_table_name(cat))

  for (sca in sca_options)
  {
    scaled <- combined %>% do_scal(sca, .) %>% do_norm("Global Min-Max", .)

    local_lower <- binary_search(scaled, upper, num_digits-1)
    local_upper <- binary_search(scaled, lower, num_digits-1)

    if (local_lower > 1 - 10^(1-num_digits))
      local_lower <- 1 - 10^(1-num_digits)

    if (local_upper < local_lower + 10^(1-num_digits))
      local_upper <- local_lower + 10^(1-num_digits)

    cat_f("(%.4f, %.4f) for %s.%s\n", local_lower, local_upper, cat, sca)

    diff <- (local_upper - local_lower)/len_inter
    thresholds <- round(seq(local_lower, local_upper, diff), num_digits)

    for (thr in thresholds)
    {
      for (cha in short_list)
      {
        make_sets_req_keys(
          cat, sca, thr, cha
        ) %>% sets_req_append()
      }
    }
  }
}

sets_req_keys <- do.call(rbind, sets_req_list)
sets_requests <- make_requests(sets_req_keys, "ADMIN")

# -----------------
# PERFORM REDUCTION
# -----------------

mnist_requests <- rbind_req(
  pca_requests,
  vae_requests,
  umap_requests,
  phate_requests,
  sets_requests
)

# save_local(1, "mnist_test.rds")
# save_local(mnist_requests, "app_requests.rds")
mnist_requests <- load_local("app_requests.rds")
include <- (
  mnist_requests$SCALING != "Linear" &
  mnist_requests$EMBEDDING != "PHATE"
)
perform_reduction(mnist_requests[include, ])
mnist_requests_fin <- req_label_times(mnist_requests)
# save_local(mnist_requests_fin, "app_requests.rds")
# save_cloud(mnist_requests_fin, "app_requests.rds")
include_sync <- mnist_requests_fin$FILE_LOCATION %in% list_local()
# copy_local_to_cloud(mnist_requests_fin$FILE_LOCATION[include_sync])
