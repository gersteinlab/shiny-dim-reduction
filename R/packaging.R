# The goal of this script is to convert vis data to packaged data.
# The purpose of this file is to generate the base set of requests.

# record: all data uploaded for the first time on v6, Feb 5, 2022.
# updating Ran's data: Feb X, 2022.

if (!exists("ran_install"))
{
  if (file.exists("install.R"))
    source("install.R")
  else
    stop("Could not confirm installation. Please source install.R manually.")
}

workflow_name <- "exRNA"
source_sdr("red_requests.R")

# simplifying perplexity for now
perplexity_types <- c(10, 50)
# setwd(dep_loc)
# saveRDS(perplexity_types, "perplexity_types.rds")

setwd(pro_loc)

# searches for a threshold to num_digits precision such that
# ncol(table_to_sets(data, thre)) approximates target
binary_search <- function(data, target, num_digits)
{
  precision <- 0.1^num_digits
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

  round((lower+upper)/2,num_digits)
}

# temporarily reduce number of normalizations
temp_nor <- nor_options[1:2]
# name_cat <- "miRNA"

# ------------
# PCA REQUESTS
# ------------

pca_requests <- make_requests()

for (cat in name_cat)
{
  for (row in sub_row_groups[[cat]])
  {
    for (col in sub_col_groups[[cat]])
    {
      for (sca in sca_options)
      {
        for (nor in temp_nor)
        {
          # explore
          pca_e <- make_pvu_requests(cat, row, col, sca, nor, "PCA", "Explore",
                                     10, num_d(), num_d(), num_d(), aut_d())
          pca_requests <- rbind(pca_requests, pca_e)

          # summarize
          pca_s <- make_pvu_requests(cat, row, col, sca, nor, "PCA", "Summarize",
                                     10, num_d(), num_d(), num_d(), aut_d())
          pca_requests <- rbind(pca_requests, pca_s)

          # tsne
          for (per in perplexity_types)
          {
            for (dim in c(2,3))
            {
              pca_t <- make_pvu_requests(cat, row, col, sca, nor, "PCA", "tSNE",
                                         10, dim, per, num_d(), aut_d())
              pca_requests <- rbind(pca_requests, pca_t)
            }
          }
        }
      }
    }
  }
}
rm(pca_e, pca_s, pca_t)

saveRDS(pca_requests, "pca_requests.rds")

# ------------
# VAE REQUESTS
# ------------

vae_requests <- make_requests()

for (cat in name_cat)
{
  for (row in sub_row_groups[[cat]])
  {
    for (col in sub_col_groups[[cat]])
    {
      for (sca in sca_options)
      {
        for (nor in temp_nor)
        {
          # explore
          vae_e <- make_pvu_requests(cat, row, col, sca, nor, "VAE", "Explore",
                                     10, num_d(), num_d(), 64, aut_d())
          vae_requests <- rbind(vae_requests, vae_e)

          # summarize
          vae_s <- make_pvu_requests(cat, row, col, sca, nor, "VAE", "Summarize",
                                     10, num_d(), num_d(), 64, aut_d())
          vae_requests <- rbind(vae_requests, vae_s)

          # tsne
          for (per in perplexity_types)
          {
            for (dim in c(2,3))
            {
              vae_t <- make_pvu_requests(cat, row, col, sca, nor, "VAE", "tSNE",
                                         10, dim, per, 64, aut_d())
              vae_requests <- rbind(vae_requests, vae_t)
            }
          }
        }
      }
    }
  }
}
rm(vae_e, vae_s, vae_t)

saveRDS(vae_requests, "vae_requests.rds")

# -------------
# UMAP REQUESTS
# -------------

umap_requests <- make_requests()

# UMAP - add missing requests from row!
for (cat in name_cat)
{
  for (row in sub_row_groups[[cat]])
  {
    for (col in sub_col_groups[[cat]])
    {
      for (sca in sca_options)
      {
        for (nor in temp_nor)
        {
          for (per in perplexity_types)
          {
            # explore
            umap_e <- make_pvu_requests(cat, row, col, sca, nor, "UMAP", "Explore",
                                        10, num_d(), per, num_d(), aut_d())
            umap_requests <- rbind(umap_requests, umap_e)

            # summarize
            umap_s <- make_pvu_requests(cat, row, col, sca, nor, "UMAP", "Summarize",
                                        10, num_d(), per, num_d(), aut_d())
            umap_requests <- rbind(umap_requests, umap_s)

            # tsne
            for (dim in c(2,3))
            {
              umap_t <- make_pvu_requests(cat, row, col, sca, nor, "UMAP", "tSNE",
                                          10, dim, per, num_d(), aut_d())
              umap_requests <- rbind(umap_requests, umap_t)
            }
          }
        }
      }
    }
  }
}
rm(umap_e, umap_s, umap_t)

saveRDS(umap_requests, "umap_requests.rds")

# --------------
# PHATE REQUESTS
# --------------

phate_requests <- make_requests()

for (cat in name_cat)
{
  for (row in sub_row_groups[[cat]])
  {
    for (col in sub_col_groups[[cat]])
    {
      for (sca in sca_options)
      {
        for (nor in temp_nor)
        {
          for (per in perplexity_types)
          {
            for (com in c(2,3))
            {
              phate_r <- make_phate_requests(cat, row, col, sca, nor, com, per, aut_d())
              phate_requests <- rbind(phate_requests, phate_r)
            }
          }
        }
      }
    }
  }
}
rm(phate_r)

saveRDS(phate_requests, "phate_requests.rds")

# -------------
# SETS REQUESTS
# -------------

# idea for summary: line graph, number of above-threshold features vs threshold (0 to 1)
setwd(pro_loc)

lower <- 8 # 2^3
upper <- 262144 # 2^18
len_inter <- 10

sets_requests <- make_requests()

for (cat in name_cat)
{
  short_list <- select_chars(order_total[[cat]])
  combined <- readRDS(sprintf("combined/combined_%s.rds", cat))

  for (sca in sca_options)
  {
    scaled <- combined %>% do_scal(sca, .) %>% do_norm("Global Min-Max", .)

    local_lower <- binary_search(scaled, upper, num_digits-1)
    local_upper <- binary_search(scaled, lower, num_digits-1)

    if (local_lower > 1 - 10^(1-num_digits))
      local_lower <- 1 - 10^(1-num_digits)

    if (local_upper < local_lower + 10^(1-num_digits))
      local_upper <- local_lower + 10^(1-num_digits)

    print(sprintf("(%s, %s) for %s %s", local_lower, local_upper, cat, sca))

    diff <- (local_upper - local_lower)/len_inter
    thresholds <- round(seq(local_lower, local_upper, diff), num_digits)

    for (thr in thresholds)
    {
      for (cha in colnames(short_list))
      {
        sets_r <- make_sets_requests(cat, sca, thr, cha, aut_d())
        sets_requests <- rbind(sets_requests, sets_r)
      }
    }
  }
}
rm(sets_r, diff, thresholds, local_lower, local_upper)

saveRDS(sets_requests, "sets_requests.rds")

# -----------------
# PERFORM REDUCTION
# -----------------

# saveRDS(pca_requests, "mirna_ran_pca_requests.rds")
# saveRDS(vae_requests, "mirna_ran_vae_requests.rds")
# saveRDS(umap_requests, "mirna_ran_umap_requests.rds")
# saveRDS(phate_requests, "mirna_ran_phate_requests.rds")
# saveRDS(sets_requests, "mirna_ran_sets_requests.rds")
pca_requests_ran <- readRDS("mirna_ran_pca_requests.rds")
vae_requests_ran <- readRDS("mirna_ran_vae_requests.rds")
umap_requests_ran <- readRDS("mirna_ran_umap_requests.rds")
phate_requests_ran <- readRDS("mirna_ran_phate_requests.rds")
sets_requests_ran <- readRDS("mirna_ran_sets_requests.rds")

# done_ran_umap <- perform_reduction(umap_requests_ran, force = 2)
# saveRDS(done_ran_umap, "done_ran_umap.rds")
# done_ran_pca <- perform_reduction(pca_requests_ran, force = 2)
# saveRDS(done_ran_pca, "done_ran_pca.rds")
# done_ran_sets <- perform_reduction(sets_requests_ran, force = 2)
# saveRDS(done_ran_sets, "done_ran_sets.rds")
# done_ran_phate <- perform_reduction(phate_requests_ran, force = 2)
# saveRDS(done_ran_phate, "done_ran_phate.rds")
# done_ran_vae <- perform_reduction(vae_requests_ran, force = 2)
# saveRDS(done_ran_vae, "done_ran_vae.rds")

done_ran_pca <- readRDS("done_ran_pca.rds")
done_ran_vae <- readRDS("done_ran_vae.rds")
done_ran_umap <- readRDS("done_ran_umap.rds")
done_ran_phate <- readRDS("done_ran_phate.rds")
done_ran_sets <- readRDS("done_ran_sets.rds")

pca_requests <- readRDS("pca_requests.rds")
vae_requests <- readRDS("vae_requests.rds")
umap_requests <- readRDS("umap_requests.rds")
phate_requests <- readRDS("phate_requests.rds")
sets_requests <- readRDS("sets_requests.rds")

system.time(ran_mirna_new <- rbind_req(
  done_ran_pca,
  done_ran_vae,
  done_ran_umap,
  done_ran_phate,
  done_ran_sets
))

ran_mirna_new$REQUEST_ID <- get_request_id(nrow(ran_mirna_new))
# saveRDS(ran_mirna_new, "ran_mirna_new.rds")
ran_mirna_new <- readRDS("ran_mirna_new.rds")

# small_test <- pca_requests[61:70,]
# small_t0 <- perform_reduction(small_test, 0)
# small_t1 <- perform_reduction(small_test, 1)
# small_t2 <- perform_reduction(small_test, 2)
# nrow(pca_requests) + nrow(vae_requests) + nrow(umap_requests) +
#   nrow(phate_requests) + nrow(sets_requests)

# fix: add FILE_LOCATION as a column to all previous requests
# for (a_name in c("done_pca_1.rds", "done_pca_2.rds", "done_vae_1.rds",
#                  "done_vae_2.rds", "done_vae_3.rds", "done_vae_4.rds",
#                  "done_vae_5.rds", "done_vae_6.rds", "done_vae_7.rds",
#                  "done_vae_8.rds", "done_vae_9.rds", "done_vae_10.rds",
#                  "done_vae_11.rds", "done_umap_1.rds", "done_umap_2.rds",
#                  "done_umap_3.rds", "done_umap_4.rds", "done_umap_5.rds",
#                  "done_umap_6.rds", "done_umap_7.rds", "done_umap_8.rds",
#                  "done_phate_1.rds", "done_phate_2.rds", "done_phate_3.rds",
#                  "done_phate_4.rds", "done_phate_5.rds", "done_phate_6.rds",
#                  "done_sets_1.rds", "done_sets_2.rds"))
# {
#   temp_requests <- readRDS(a_name)
#   temp_requests$FILE_LOCATION <- requests_to_final(temp_requests)
#   saveRDS(temp_requests, a_name)
# }

# system.time(app_requests <- rbind_req(
#   readRDS("done_pca_1.rds"),
#   readRDS("done_pca_2.rds"),
#   readRDS("done_vae_1.rds"),
#   readRDS("done_vae_2.rds"),
#   readRDS("done_vae_3.rds"),
#   readRDS("done_vae_4.rds"),
#   readRDS("done_vae_5.rds"),
#   readRDS("done_vae_6.rds"),
#   readRDS("done_vae_7.rds"),
#   readRDS("done_vae_8.rds"),
#   readRDS("done_vae_9.rds"),
#   readRDS("done_vae_10.rds"),
#   readRDS("done_vae_11.rds"),
#   readRDS("done_umap_1.rds"),
#   readRDS("done_umap_2.rds"),
#   readRDS("done_umap_3.rds"),
#   readRDS("done_umap_4.rds"),
#   readRDS("done_umap_5.rds"),
#   readRDS("done_umap_6.rds"),
#   readRDS("done_umap_7.rds"),
#   readRDS("done_umap_8.rds"),
#   readRDS("done_phate_1.rds"),
#   readRDS("done_phate_2.rds"),
#   readRDS("done_phate_3.rds"),
#   readRDS("done_phate_4.rds"),
#   readRDS("done_phate_5.rds"),
#   readRDS("done_phate_6.rds"),
#   readRDS("done_sets_1.rds"),
#   readRDS("done_sets_2.rds")
# ))
#
# app_requests$REQUEST_ID <- get_request_id(nrow(app_requests))
# saveRDS(app_requests, "app_requests.rds")

app_requests_1_23 <- readRDS("app_requests_1_23.rds")

app_requests_2_16 <- app_requests_1_23
app_requests_2_16 <- app_requests_2_16[app_requests_2_16$CATEGORIES != "miRNA",]
app_requests_2_16 <- rbind_req(app_requests_2_16, ran_mirna_new)

saveRDS(app_requests_2_16, "app_requests_2_16.rds")

setwd(ref_loc)
saveRDS(app_requests_2_16, "app_requests.rds")

# syncs a set of requests from reference to AWS
sudo_working_key(amazon_keys)
setwd(ref_loc)
app_requests <- readRDS("app_requests.rds")
existing_files <- list_aws_s3()
request_files <- app_requests$FILE_LOCATION
# files_to_be_uploaded <- setdiff(request_files, existing_files)
files_to_be_uploaded <- app_requests$FILE_LOCATION[
  app_requests$CATEGORIES == "miRNA" & app_requests$EMBEDDING == "Sets"]

n <- length(files_to_be_uploaded)

my_range <- seq_len(n)
set_working_ref(ref_loc)
# my_range <- 39401:n
# my_range <- 62021:n

for (i in my_range)
{
  if (i %% 100 == 0 || i == n)
  {
    sprintf_clean("Synced %s out of %s", i, n)
  }
  single_file <- files_to_be_uploaded[i]
  data <- load_local(single_file)
  if (is.null(data))
    stop(sprintf("Cannot load data entry %s to sync!", i))
  save_aws_s3(data, single_file)
}

save_aws_s3(app_requests, "app_requests.rds")
