# The goal of this script is to convert vis data to packaged data.
# The purpose of this file is to generate the base set of requests.

if (!exists("ran_install"))
{
  if (file.exists("install.R"))
    source("install.R")
  else
    stop("Could not confirm installation. Please source install.R manually.")
}

source_sdr("red_requests.R")

# simplifying perplexity for now
# setwd(dep_loc)
# saveRDS(c(10, 50), "perplexity_types.rds")

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

# searches for a threshold to numdigits precision
# such that table_to_sets(data, thre) approximates target
binary_search <- function(data, target, numdigits)
{
  precision <- 0.1^numdigits
  lower <- precision
  upper <- 1
  while (upper-lower >= precision)
  {
    mid <- (lower + upper)/2

    if (sum(colSums(data >= mid) > 0) < target)
      upper <- mid
    else
      lower <- mid
  }

  round((lower+upper)/2,numdigits)
}

# -----------------
# GENERATE REQUESTS
# -----------------

start <- my_timer()

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
          pca_s <- make_pvu_requests(cat, row, col, sca, nor, "PCA", "Summarize",
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

# VAE
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
          vae_e <- make_pvu_requests(cat, row, col, sca, nor, "VAE", "Explore",
                                     10, num_d(), num_d(), 64, aut_d())
          master_requests <- rbind(master_requests, vae_e)

          # summarize
          vae_s <- make_pvu_requests(cat, row, col, sca, nor, "VAE", "Summarize",
                                     10, num_d(), num_d(), 64, aut_d())
          master_requests <- rbind(master_requests, vae_s)

          # tsne
          for (per in perplexity_types)
          {
            for (dim in c(2,3))
            {
              vae_t <- make_pvu_requests(cat, row, col, sca, nor, "VAE", "tSNE",
                                         10, dim, per, 64, aut_d())
              master_requests <- rbind(master_requests, vae_t)
            }
          }
        }
      }
    }
  }
}

# UMAP
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
          for (per in perplexity_types)
          {
            # explore
            umap_e <- make_pvu_requests(cat, row, col, sca, nor, "UMAP", "Explore",
                                        10, num_d(), per, num_d(), aut_d())
            master_requests <- rbind(master_requests, umap_e)

            # summarize
            umap_s <- make_pvu_requests(cat, row, col, sca, nor, "UMAP", "Summarize",
                                        10, num_d(), per, num_d(), aut_d())
            master_requests <- rbind(master_requests, umap_s)

            # tsne
            for (dim in c(2,3))
            {
              umap_t <- make_pvu_requests(cat, row, col, sca, nor, "UMAP", "tSNE",
                                          10, dim, per, num_d(), aut_d())
              master_requests <- rbind(master_requests, umap_t)
            }
          }
        }
      }
    }
  }
}

# PHATE
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
          for (per in perplexity_types)
          {
            for (com in c(2,3))
            {
              phate_r <- make_phate_requests(cat, row, col, sca, nor, com, per, aut_d())
              master_requests <- rbind(master_requests, phate_r)
            }
          }
        }
      }
    }
  }
}

# Sets
setwd(pro_loc)

lower <- 8 # 2^3
upper <- 262144 # 2^18
numdigits <- 4
len_inter <- 10

for (cat in name_cat)
{
  short_list <- select_chars(order_total[[cat]])
  combined <- readRDS(sprintf("combined/combined_%s.rds", cat))

  for (sca in sca_options)
  {
    scaled <- combined %>% do_scal(sca, .) %>% do_norm(nor_options[1], .)

    local_lower <- binary_search(scaled, upper, numdigits-1)
    local_upper <- binary_search(scaled, lower, numdigits-1)

    if (local_lower > 1 - 10^(1-numdigits))
      local_lower <- 1 - 10^(1-numdigits)

    if (local_upper < local_lower + 10^(1-numdigits))
      local_upper <- local_lower + 10^(1-numdigits)

    print(sprintf("(%s, %s) for %s %s", local_lower, local_upper, cat, sca))

    diff <- (local_upper - local_lower)/len_inter
    thresholds <- round(seq(local_lower, local_upper, diff), numdigits)

    for (thr in thresholds)
    {
      for (cha in colnames(short_list))
      {
        sets_r <- make_sets_requests(cat, sca, thr, cha, aut_d())
        master_requests <- rbind(master_requests, sets_r)
      }
    }
  }
}

end <- my_timer(start)
sprintf_clean("Time elapsed in seconds: %s", end) # 394.4389 seconds
sum(master_requests$CATEGORIES == "miRNA" & master_requests$EMBEDDING == "PCA") # 360

# subset <- master_requests$EMBEDDING == "PCA"
# subset_req <- perform_reduction(master_requests[subset,], 0)

# if (!exists("ran_install"))
# {
#   if (file.exists("install.R"))
#     source("install.R")
#   else
#     stop("Could not confirm installation. Please source install.R manually.")
# }
#
# source_sdr("storage.R")
# source_sdr("preprocess.R")
#
# setwd(app_loc)
# get_from_dir("amazon_keys")
#
# set_working_key(amazon_keys)
# sudo_working_key()
# storage_query()
#
# # -----------
# # PACKAGE PCA
# # -----------
# setwd(pro_loc)
# dog <- name_cat
# emb <- "PCA"
# derp <- matrix(0, nrow = 100, ncol = 9)
# ind <- 1
# for (cat in dog)
# {
#   print(cat)
#   print(Sys.time())
#
#   for (sub in sub_groups[[cat]])
#   {
#     for (sca in sca_options)
#     {
#       for (nor in nor_options)
#       {
#         for (fea in c(1, 10, 100))
#         {
#           loc <- sprintf("%s_%s_%s_%s_%s.rds", fea, nor, sca, sub, cat)
#
#           derp[ind,] <- c(cat, sub, sca, nor, fea, emb, "Explore", "", "")
#           ind <- ind + 1
#           # none <- readRDS(sprintf("vis-%s/NONE_%s_%s", emb, emb, loc))
#           # save_store(
#           #   none,
#           #   make_aws_name(cat, sub, sca, nor, fea, emb, "Explore", "", "")
#           # )
#           #
#           # sum <- readRDS(sprintf("vis-%s/SUM_%s_%s", emb, emb, loc))
#           # save_store(
#           #   sum,
#           #   make_aws_name(cat, sub, sca, nor, fea, emb, "Summarize", "", "")
#           # )
#           #
#           # tsne_vis <- readRDS(sprintf("vis-%s/TSNE_%s_%s", emb, emb, loc))
#           #
#           # for (nei in perplexity_types)
#           # {
#           #   nei_ind <- which(perplexity_types == nei)
#           #
#           #   for (dim in c(2,3))
#           #   {
#           #     save_store(
#           #       tsne_vis[[sprintf("TSNE%s", dim)]][[sprintf("P%s", nei)]],
#           #       make_aws_name(cat, sub, sca, nor, fea, emb, "tSNE", dim, nei_ind)
#           #     )
#           #   }
#           # }
#         }
#       }
#     }
#   }
# }
#
# # -----------
# # PACKAGE VAE
# # -----------
# setwd(pro_loc)
# dog <- name_cat
# emb <- "VAE"
# for (cat in dog)
# {
#   print(cat)
#   print(Sys.time())
#
#   for (sub in sub_groups[[cat]])
#   {
#     for (sca in sca_options)
#     {
#       for (nor in nor_options[1:2])
#       {
#         for (fea in c(1, 10, 100))
#         {
#           loc <- sprintf("%s_%s_%s_%s_%s.rds", fea, nor, sca, sub, cat)
#
#           none <- readRDS(sprintf("vis-%s/NONE_%s_%s", emb, emb, loc))
#           save_store(
#             none,
#             make_aws_name(cat, sub, sca, nor, fea, emb, "Explore", "", "")
#           )
#
#           sum <- readRDS(sprintf("vis-%s/SUM_%s_%s", emb, emb, loc))
#           save_store(
#             sum,
#             make_aws_name(cat, sub, sca, nor, fea, emb, "Summarize", "", "")
#           )
#
#           tsne_vis <- readRDS(sprintf("vis-%s/TSNE_%s_%s", emb, emb, loc))
#
#           for (nei in perplexity_types)
#           {
#             nei_ind <- which(perplexity_types == nei)
#
#             for (dim in c(2,3))
#             {
#               save_store(
#                 tsne_vis[[sprintf("TSNE%s", dim)]][[sprintf("P%s", nei)]],
#                 make_aws_name(cat, sub, sca, nor, fea, emb, "tSNE", dim, nei_ind)
#               )
#             }
#           }
#         }
#       }
#     }
#   }
# }
#
# # ------------
# # PACKAGE UMAP
# # ------------
# setwd(pro_loc)
# dog <- name_cat
# for (cat in dog)
# {
#   print(cat)
#   print(Sys.time())
#
#   for (sub in sub_groups[[cat]])
#   {
#     for (sca in sca_options)
#     {
#       for (nor in nor_options)
#       {
#         for (fea in c(1, 10, 100))
#         {
#           loc <- sprintf("%s_%s_%s_%s_%s.rds", fea, nor, sca, sub, cat)
#
#           sum <- readRDS(sprintf("vis-UMAP/SUM_UMAP_%s", loc))
#           save_store(
#             sum,
#             make_aws_name(cat, sub, sca, nor, fea, "UMAP", "Summarize", "", "")
#           )
#
#           explore <- readRDS(sprintf("vis-UMAP/NONE_UMAP_%s", loc))
#           tsne_vis <- readRDS(sprintf("vis-UMAP/TSNE_UMAP_%s", loc))
#
#           for (nei in perplexity_types)
#           {
#             nei_ind <- which(perplexity_types == nei)
#
#             save_store(
#               explore[[sprintf("P%s", nei)]],
#               make_aws_name(cat, sub, sca, nor, fea, "UMAP", "Explore", "", nei_ind)
#             )
#
#             for (dim in c(2,3))
#             {
#               save_store(
#                 tsne_vis[[sprintf("TSNE%s", dim)]][[sprintf("P%s", nei)]],
#                 make_aws_name(cat, sub, sca, nor, fea, "UMAP", "tSNE", dim, nei_ind)
#               )
#             }
#           }
#         }
#       }
#     }
#   }
# }
#
# # -------------
# # PACKAGE PHATE
# # -------------
# dog <- names(categories)
# setwd(pro_loc)
# for (cat in dog)
# {
#   print(cat)
#   print(Sys.time())
#
#   for (sub in sub_groups[[cat]])
#   {
#     for (sca in sca_options)
#     {
#       for (nor in nor_options)
#       {
#         for (fea in c(1, 10, 100))
#         {
#           for (nei in perplexity_types)
#           {
#             nei_ind <- which(perplexity_types == nei)
#
#             for (dim in c(2,3))
#             {
#               phate <- readRDS(sprintf(
#                 "PHATE/PHATE-%s-%s_%s_%s_%s_%s_%s.rds",
#                 nei, dim, fea, nor, sca, sub, cat))$embedding
#
#               save_store(
#                 phate,
#                 make_aws_name(cat, sub, sca, nor, fea, "PHATE", "", dim, nei_ind)
#               )
#             }
#           }
#         }
#       }
#     }
#   }
# }
#
# # ------------
# # PACKAGE SETS
# # ------------
# dog <- names(categories)
# num_filters <- 60
# setwd(pro_loc)
# for (cat in dog)
# {
#   print(cat)
#   print(Sys.time())
#
#   order <- order_total[[cat]]
#
#   short_list <- select_if(order, function(x){
#     between(length(unique(x)), 2, num_filters)
#   })
#
#   for (sca in sca_options)
#   {
#     sca_ind <- which(sca_options == sca)
#
#     for (ind in 11:1)
#     {
#       set_data <- readRDS(sprintf("Sets/Sets-%s_%s_%s.rds",
#                                   ind, sca, cat))
#
#       for (cha in colnames(short_list))
#       {
#         save_store(
#           set_data[[cha]],
#           sprintf("Sets/Sets-%s_%s_%s_%s.rds",
#                   ind, sca_ind, cha, cat)
#         )
#       }
#     }
#   }
# }
