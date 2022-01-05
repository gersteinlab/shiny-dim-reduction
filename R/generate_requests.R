# The purpose of this file is to generate the base set of requests.

if (!exists("ran_install"))
{
  if (file.exists("install.R"))
    source("install.R")
  else
    stop("Could not confirm installation. Please source install.R manually.")
}

source_sdr("perform_reduction.R")

# -------------
# EMB FUNCTIONS
# -------------

# pca, vae, umap
make_pvu_requests <- function(
  cat = character(), row = character(), col = character(), sca = character(),
  nor = character(), emb = character(), vis = character(), com = numeric(),
  dim = numeric(), per = numeric(), bat = numeric(), aut = character()
)
{
  n_cat <- length(cat)

  make_requests(
    cat, row, col, sca, nor, emb, vis,
    com, dim, per, bat, num_d(n_cat), chr_d(n_cat), aut)
}

# simplifies the generation of PHATE requests
make_phate_requests <- function(
  cat = character(), row = character(), col = character(), sca = character(),
  nor = character(), com = numeric(), per = numeric(), aut = character()
)
{
  n_cat <- length(cat)

  make_requests(
    cat, row, col, sca, nor, rep("PHATE", n_cat), chr_d(n_cat),
    com, num_d(n_cat), per, num_d(n_cat), num_d(n_cat), chr_d(n_cat), aut)
}

# simplifies the generation of Sets requests
make_sets_requests <- function(
  cat = character(), sca = character(), thr = numeric(), cha = character(), aut = character())
{
  n_cat <- length(cat)

  make_requests(
    cat, chr_d(n_cat), chr_d(n_cat), sca, rep("Global Min-Max", n_cat), rep("Sets", n_cat), chr_d(n_cat),
    num_d(n_cat), num_d(n_cat), num_d(n_cat), num_d(n_cat), thr, cha, aut)
}

# -----------------
# GENERATE REQUESTS
# -----------------

master_requests <- make_requests()

# PCA
for (cat in name_cat)
{
  for (row in sub_row_groups[[cat]])
  {
    for (col in sub_col_groups[[cat]])
    {
      for (sca in sca_options)
      {
        for (nor in nor_options)
        {
          # explore
          pca_e <- make_pvu_requests(cat, row, col, sca, nor, "PCA", "Explore",
                                     10, num_d(), num_d(), num_d(), aut_d())
          master_requests <- rbind(master_requests, pca_e)

          # summarize
          pca_s <- make_pvu_requests(cat, row, col, sca, nor, "PCA", "Explore",
                                     10, num_d(), num_d(), num_d(), aut_d())
          master_requests <- rbind(master_requests, pca_s)

          # tsne
          for (per in perplexity_types)
          {
            for (dim in c(2,3))
            {
              pca_t <- make_pvu_requests(cat, row, col, sca, nor, "PCA", "tSNE",
                                         10, dim, per, num_d(), aut_d())
              master_requests <- rbind(master_requests, pca_t)
            }
          }
        }
      }
    }
  }
}
