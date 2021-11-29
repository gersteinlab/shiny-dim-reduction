# The goal of this script is to convert vis data to packaged data.

if (!exists("ran_install"))
{
  if (file.exists("install.R"))
    source("install.R")
  else
    stop("Could not confirm installation. Please source install.R manually.")
}

source_sdr("storage.R")
source_sdr("preprocess.R")

setwd(app_loc)
get_from_dir("amazon_keys")

# assign keys to admin plus decided bucket
load_master_key(amazon_keys$bucket)
set_working_key(master_key)
storage_query()

# -----------
# PACKAGE PCA
# -----------
setwd(pro_loc)
dog <- name_cat
emb <- "PCA"
derp <- matrix(0, nrow = 100, ncol = 9)
ind <- 1
for (cat in dog)
{
  print(cat)
  print(Sys.time())

  for (sub in sub_groups[[cat]])
  {
    for (sca in sca_options)
    {
      for (nor in nor_options)
      {
        for (fea in c(1, 10, 100))
        {
          loc <- sprintf("%s_%s_%s_%s_%s.rds", fea, nor, sca, sub, cat)

          derp[ind,] <- c(cat, sub, sca, nor, fea, emb, "Explore", "", "")
          ind <- ind + 1
          # none <- readRDS(sprintf("vis-%s/NONE_%s_%s", emb, emb, loc))
          # save_store(
          #   none,
          #   make_aws_name(cat, sub, sca, nor, fea, emb, "Explore", "", "")
          # )
          #
          # sum <- readRDS(sprintf("vis-%s/SUM_%s_%s", emb, emb, loc))
          # save_store(
          #   sum,
          #   make_aws_name(cat, sub, sca, nor, fea, emb, "Summarize", "", "")
          # )
          #
          # tsne_vis <- readRDS(sprintf("vis-%s/TSNE_%s_%s", emb, emb, loc))
          #
          # for (nei in perplexity_types)
          # {
          #   nei_ind <- which(perplexity_types == nei)
          #
          #   for (dim in c(2,3))
          #   {
          #     save_store(
          #       tsne_vis[[sprintf("TSNE%s", dim)]][[sprintf("P%s", nei)]],
          #       make_aws_name(cat, sub, sca, nor, fea, emb, "tSNE", dim, nei_ind)
          #     )
          #   }
          # }
        }
      }
    }
  }
}

# -----------
# PACKAGE VAE
# -----------
setwd(pro_loc)
dog <- name_cat
emb <- "VAE"
for (cat in dog)
{
  print(cat)
  print(Sys.time())

  for (sub in sub_groups[[cat]])
  {
    for (sca in sca_options)
    {
      for (nor in nor_options[1:2])
      {
        for (fea in c(1, 10, 100))
        {
          loc <- sprintf("%s_%s_%s_%s_%s.rds", fea, nor, sca, sub, cat)

          none <- readRDS(sprintf("vis-%s/NONE_%s_%s", emb, emb, loc))
          save_store(
            none,
            make_aws_name(cat, sub, sca, nor, fea, emb, "Explore", "", "")
          )

          sum <- readRDS(sprintf("vis-%s/SUM_%s_%s", emb, emb, loc))
          save_store(
            sum,
            make_aws_name(cat, sub, sca, nor, fea, emb, "Summarize", "", "")
          )

          tsne_vis <- readRDS(sprintf("vis-%s/TSNE_%s_%s", emb, emb, loc))

          for (nei in perplexity_types)
          {
            nei_ind <- which(perplexity_types == nei)

            for (dim in c(2,3))
            {
              save_store(
                tsne_vis[[sprintf("TSNE%s", dim)]][[sprintf("P%s", nei)]],
                make_aws_name(cat, sub, sca, nor, fea, emb, "tSNE", dim, nei_ind)
              )
            }
          }
        }
      }
    }
  }
}

# columns:
# name
# date
# source
# row subset
# col subset
# scaling
# normalization

# embedding
# visualization
# dimension (not user-filterable; depends on source)
# perplexity

records <- data.frame()

# ------------
# PACKAGE UMAP
# ------------
setwd(pro_loc)
dog <- name_cat
for (cat in dog)
{
  print(cat)
  print(Sys.time())

  for (sub in sub_groups[[cat]])
  {
    for (sca in sca_options)
    {
      for (nor in nor_options)
      {
        for (fea in c(1, 10, 100))
        {
          loc <- sprintf("%s_%s_%s_%s_%s.rds", fea, nor, sca, sub, cat)

          sum <- readRDS(sprintf("vis-UMAP/SUM_UMAP_%s", loc))
          save_store(
            sum,
            make_aws_name(cat, sub, sca, nor, fea, "UMAP", "Summarize", "", "")
          )

          explore <- readRDS(sprintf("vis-UMAP/NONE_UMAP_%s", loc))
          tsne_vis <- readRDS(sprintf("vis-UMAP/TSNE_UMAP_%s", loc))

          for (nei in perplexity_types)
          {
            nei_ind <- which(perplexity_types == nei)

            save_store(
              explore[[sprintf("P%s", nei)]],
              make_aws_name(cat, sub, sca, nor, fea, "UMAP", "Explore", "", nei_ind)
            )

            for (dim in c(2,3))
            {
              save_store(
                tsne_vis[[sprintf("TSNE%s", dim)]][[sprintf("P%s", nei)]],
                make_aws_name(cat, sub, sca, nor, fea, "UMAP", "tSNE", dim, nei_ind)
              )
            }
          }
        }
      }
    }
  }
}

# -------------
# PACKAGE PHATE
# -------------
dog <- names(categories)
setwd(pro_loc)
for (cat in dog)
{
  print(cat)
  print(Sys.time())

  for (sub in sub_groups[[cat]])
  {
    for (sca in sca_options)
    {
      for (nor in nor_options)
      {
        for (fea in c(1, 10, 100))
        {
          for (nei in perplexity_types)
          {
            nei_ind <- which(perplexity_types == nei)

            for (dim in c(2,3))
            {
              phate <- readRDS(sprintf(
                "PHATE/PHATE-%s-%s_%s_%s_%s_%s_%s.rds",
                nei, dim, fea, nor, sca, sub, cat))$embedding

              save_store(
                phate,
                make_aws_name(cat, sub, sca, nor, fea, "PHATE", "", dim, nei_ind)
              )
            }
          }
        }
      }
    }
  }
}

# ------------
# PACKAGE SETS
# ------------
dog <- names(categories)
num_filters <- 60
setwd(pro_loc)
for (cat in dog)
{
  print(cat)
  print(Sys.time())

  order <- order_total[[cat]]

  short_list <- select_if(order, function(x){
    between(length(unique(x)), 2, num_filters)
  })

  for (sca in sca_options)
  {
    sca_ind <- which(sca_options == sca)

    for (ind in 11:1)
    {
      set_data <- readRDS(sprintf("Sets/Sets-%s_%s_%s.rds",
                                  ind, sca, cat))

      for (cha in colnames(short_list))
      {
        save_store(
          set_data[[cha]],
          sprintf("Sets/Sets-%s_%s_%s_%s.rds",
                  ind, sca_ind, cha, cat)
        )
      }
    }
  }
}
