# The goal of this script is to convert vis data to packaged data.
# The purpose of this file is to generate the base set of requests.

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

# searches for a threshold to num_digits precision
# such that table_to_sets(data, thre) approximates target
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

# -----------------
# GENERATE REQUESTS
# -----------------

start <- my_timer()
pca_requests_new <- make_requests()

# PCA
for (cat in name_cat)
{
  # used to patch missing entries
  temp_row <- setdiff(sub_row_groups[[cat]], "Total")
  if (length(temp_row) > 0)
    temp_row <- temp_row[1]

  for (row in temp_row)
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
          pca_requests_new <- rbind(pca_requests_new, pca_e)

          # summarize
          pca_s <- make_pvu_requests(cat, row, col, sca, nor, "PCA", "Summarize",
                                     10, num_d(), num_d(), num_d(), aut_d())
          pca_requests_new <- rbind(pca_requests_new, pca_s)

          # tsne
          for (per in perplexity_types)
          {
            for (dim in c(2,3))
            {
              pca_t <- make_pvu_requests(cat, row, col, sca, nor, "PCA", "tSNE",
                                         10, dim, per, num_d(), aut_d())
              pca_requests_new <- rbind(pca_requests_new, pca_t)
            }
          }
        }
      }
    }
  }
}

pca_end <- my_timer(start)
# saveRDS(pca_requests, "pca_requests.rds")
pca_requests <- readRDS("pca_requests.rds")
# saveRDS(pca_requests_new, "pca_requests_new.rds")
pca_requests_new <- readRDS("pca_requests_new.rds")
# done_pca1 <- perform_reduction(pca_requests)
# saveRDS(done_pca1, "done_pca_1.rds")
# done_pca2 <- perform_reduction(pca_requests_new)
# saveRDS(done_pca2, "done_pca_2.rds")

# Note: Due to incorrect row subset generation, we are missing some PCA samples!
# View(pca_requests[pca_requests$CATEGORIES == "miRNA" &
#   pca_requests$ROW_SUBSETS == "Unknown",])

start <- my_timer()
vae_requests_new <- make_requests()

# VAE
for (cat in name_cat)
{
  # used to patch missing entries
  temp_row <- setdiff(sub_row_groups[[cat]], "Total")
  if (length(temp_row) > 0)
    temp_row <- temp_row[1]

  for (row in temp_row)
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
          vae_requests_new <- rbind(vae_requests_new, vae_e)

          # summarize
          vae_s <- make_pvu_requests(cat, row, col, sca, nor, "VAE", "Summarize",
                                     10, num_d(), num_d(), 64, aut_d())
          vae_requests_new <- rbind(vae_requests_new, vae_s)

          # tsne
          for (per in perplexity_types)
          {
            for (dim in c(2,3))
            {
              vae_t <- make_pvu_requests(cat, row, col, sca, nor, "VAE", "tSNE",
                                         10, dim, per, 64, aut_d())
              vae_requests_new <- rbind(vae_requests_new, vae_t)
            }
          }
        }
      }
    }
  }
}

vae_end <- my_timer(start)

# saveRDS(vae_requests, "vae_requests.rds")
vae_requests <- readRDS("vae_requests.rds")
# saveRDS(vae_requests_new, "vae_requests_new.rds")
vae_requests_new <- readRDS("vae_requests_new.rds")
# done_vae1 <- perform_reduction(vae_requests[vae_requests$CATEGORIES == "miRNA",])
# saveRDS(done_vae1, "done_vae_1.rds")
# done_vae2 <- perform_reduction(vae_requests[vae_requests$CATEGORIES == "piRNA",])
# saveRDS(done_vae2, "done_vae_2.rds")
# done_vae3 <- perform_reduction(vae_requests[vae_requests$CATEGORIES == "ex_miRNA",])
# saveRDS(done_vae3, "done_vae_3.rds")
# sub4 <- vae_requests$CATEGORIES %in% c(
#   "tRNA", "circRNA", "RNA_binding_proteins")
# done_vae4 <- perform_reduction(vae_requests[sub4,])
# saveRDS(done_vae4, "done_vae_4.rds")
# sub5 <- vae_requests$CATEGORIES %in% c(
#   "cumulative_ex_ribosomes", "specific_ex_ribosomes")
# done_vae5 <- perform_reduction(vae_requests[sub5,])
# saveRDS(done_vae5, "done_vae_5.rds")
# sub6 <- vae_requests$CATEGORIES == "rRNA_Species"
# done_vae6 <- perform_reduction(vae_requests[sub6,])
# saveRDS(done_vae6, "done_vae_6.rds")
# sub7 <- vae_requests$CATEGORIES == "Gene_Species"
# done_vae7 <- perform_reduction(vae_requests[sub7,])
# saveRDS(done_vae7, "done_vae_7.rds")
# sub8 <- vae_requests$CATEGORIES %in% c("Gene_Transpose", "rRNA_Transpose")
# done_vae8 <- perform_reduction(vae_requests[sub8,])
# saveRDS(done_vae8, "done_vae_8.rds")
# sub9 <- vae_requests$CATEGORIES == "cumulative_ex_genomes"
# done_vae9 <- perform_reduction(vae_requests[sub9,])
# saveRDS(done_vae9, "done_vae_9.rds")
# sub10 <- vae_requests$CATEGORIES == "specific_ex_genomes"
# done_vae10 <- perform_reduction(vae_requests[sub10,])
# saveRDS(done_vae10, "done_vae_10.rds")
# done_vae11 <- perform_reduction(vae_requests_new)
# saveRDS(done_vae11, "done_vae_11.rds")

start <- my_timer()
new_umap_requests <- make_requests()

# UMAP - add missing requests from row!
for (cat in name_cat)
{
  # used to patch missing entries
  temp_row <- setdiff(sub_row_groups[[cat]], "Total")
  if (length(temp_row) > 0)
    temp_row <- temp_row[1]

  for (row in temp_row)
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
            new_umap_requests <- rbind(new_umap_requests, umap_e)

            # summarize
            umap_s <- make_pvu_requests(cat, row, col, sca, nor, "UMAP", "Summarize",
                                        10, num_d(), per, num_d(), aut_d())
            new_umap_requests <- rbind(new_umap_requests, umap_s)

            # tsne
            for (dim in c(2,3))
            {
              umap_t <- make_pvu_requests(cat, row, col, sca, nor, "UMAP", "tSNE",
                                          10, dim, per, num_d(), aut_d())
              new_umap_requests <- rbind(new_umap_requests, umap_t)
            }
          }
        }
      }
    }
  }
}

umap_end <- my_timer(start)
# saveRDS(new_umap_requests, "umap_requests_new.rds")
# saveRDS(umap_requests, "umap_requests.rds")
umap_requests <- readRDS("umap_requests.rds")
sub1 <- umap_requests$COL_SUBSETS != "Total" & umap_requests$ROW_SUBSETS != "Total"
# done_umap1 <- perform_reduction(umap_requests[sub1,])
# saveRDS(done_umap1, "done_umap_1.rds")
sub2 <- !sub1 & umap_requests$CATEGORIES %in% c("miRNA", "tRNA", "circRNA", "ex_miRNA")
# done_umap2 <- perform_reduction(umap_requests[sub2,])
# saveRDS(done_umap2, "done_umap_2.rds")
sub3 <- !sub1 & umap_requests$CATEGORIES == "piRNA"
# done_umap3 <- perform_reduction(umap_requests[sub3,])
# saveRDS(done_umap3, "done_umap_3.rds")
sub4 <- !sub1 & umap_requests$CATEGORIES == "cumulative_ex_genomes"
# done_umap4 <- perform_reduction(umap_requests[sub4,])
# saveRDS(done_umap4, "done_umap_4.rds")
sub5 <- !sub1 & umap_requests$CATEGORIES == "specific_ex_genomes"
# done_umap5 <- perform_reduction(umap_requests[sub5,])
# saveRDS(done_umap5, "done_umap_5.rds")
sub6 <- !sub1 & umap_requests$CATEGORIES %in% c(
  "rRNA_Transpose", "Gene_Transpose", "RNA_binding_proteins")
# done_umap6 <- perform_reduction(umap_requests[sub6,])
# saveRDS(done_umap6, "done_umap_6.rds")
sub7 <- !sub1 & umap_requests$CATEGORIES %in% c(
  "cumulative_ex_ribosomes", "specific_ex_ribosomes", "rRNA_Species", "Gene_Species")
# done_umap7 <- perform_reduction(umap_requests[sub7,])
# saveRDS(done_umap7, "done_umap_7.rds")
# done_umap8 <- perform_reduction(new_umap_requests)
# saveRDS(done_umap8, "done_umap_8.rds")

# PHATE
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

# saveRDS(phate_requests, "phate_requests.rds")
phate_requests <- readRDS("phate_requests.rds")
sub1 <- phate_requests$CATEGORIES == "miRNA"
sub2 <- phate_requests$CATEGORIES == "piRNA"
sub3 <- phate_requests$CATEGORIES == "tRNA"
sub4 <- phate_requests$CATEGORIES %in% c("RNA_binding_proteins", "rRNA_Transpose", "Gene_Transpose")
sub5 <- phate_requests$CATEGORIES %in% c(
  "circRNA", "ex_miRNA", "cumulative_ex_genomes", "specific_ex_genomes"
)
sub6 <- phate_requests$CATEGORIES %in% c(
  "cumulative_ex_ribosomes", "specific_ex_ribosomes", "rRNA_Species", "Gene_Species"
)
# done_phate1 <- perform_reduction(phate_requests[sub1,])
# saveRDS(done_phate1, "done_phate_1.rds")
# done_phate2 <- perform_reduction(phate_requests[sub2,])
# saveRDS(done_phate2, "done_phate_2.rds")
# done_phate3 <- perform_reduction(phate_requests[sub3,])
# saveRDS(done_phate3, "done_phate_3.rds")
# done_phate4 <- perform_reduction(phate_requests[sub4,])
# saveRDS(done_phate4, "done_phate_4.rds")
# done_phate5 <- perform_reduction(phate_requests[sub5,])
# saveRDS(done_phate5, "done_phate_5.rds")
# done_phate6 <- perform_reduction(phate_requests[sub6,])
# saveRDS(done_phate6, "done_phate_6.rds")

# Sets
# idea for summary: number of features at each cutoff? x-axis = cutoff, y-axis = # features
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

end <- my_timer(start)

# saveRDS(set_requests, "sets_requests.rds")
sets_requests <- readRDS("sets_requests.rds")
sub1 <- sets_requests$CATEGORIES == "miRNA"
# done_sets1 <- perform_reduction(sets_requests[sub1,])
# saveRDS(done_sets1, "done_sets_1.rds")
sub2 <- sets_requests$CATEGORIES != "miRNA"
# done_sets2 <- perform_reduction(sets_requests[sub2,])
# saveRDS(done_sets2, "done_sets_2.rds")

# fix: add FILE_LOCATION as a column to all previous requests
for (a_name in c("done_pca_1.rds", "done_pca_2.rds", "done_vae_1.rds",
                 "done_vae_2.rds", "done_vae_3.rds", "done_vae_4.rds",
                 "done_vae_5.rds", "done_vae_6.rds", "done_vae_7.rds",
                 "done_vae_8.rds", "done_vae_9.rds", "done_vae_10.rds",
                 "done_vae_11.rds", "done_umap_1.rds", "done_umap_2.rds",
                 "done_umap_3.rds", "done_umap_4.rds", "done_umap_5.rds",
                 "done_umap_6.rds", "done_umap_7.rds", "done_umap_8.rds",
                 "done_phate_1.rds", "done_phate_2.rds", "done_phate_3.rds",
                 "done_phate_4.rds", "done_phate_5.rds", "done_phate_6.rds",
                 "done_sets_1.rds", "done_sets_2.rds"))
{
  temp_requests <- readRDS(a_name)
  temp_requests$FILE_LOCATION <- requests_to_final(temp_requests)
  saveRDS(temp_requests, a_name)
}

system.time(app_requests <- rbind_req(
  readRDS("done_pca_1.rds"),
  readRDS("done_pca_2.rds"),
  readRDS("done_vae_1.rds"),
  readRDS("done_vae_2.rds"),
  readRDS("done_vae_3.rds"),
  readRDS("done_vae_4.rds"),
  readRDS("done_vae_5.rds"),
  readRDS("done_vae_6.rds"),
  readRDS("done_vae_7.rds"),
  readRDS("done_vae_8.rds"),
  readRDS("done_vae_9.rds"),
  readRDS("done_vae_10.rds"),
  readRDS("done_vae_11.rds"),
  readRDS("done_umap_1.rds"),
  readRDS("done_umap_2.rds"),
  readRDS("done_umap_3.rds"),
  readRDS("done_umap_4.rds"),
  readRDS("done_umap_5.rds"),
  readRDS("done_umap_6.rds"),
  readRDS("done_umap_7.rds"),
  readRDS("done_umap_8.rds"),
  readRDS("done_phate_1.rds"),
  readRDS("done_phate_2.rds"),
  readRDS("done_phate_3.rds"),
  readRDS("done_phate_4.rds"),
  readRDS("done_phate_5.rds"),
  readRDS("done_phate_6.rds"),
  readRDS("done_sets_1.rds"),
  readRDS("done_sets_2.rds")
))

app_requests$REQUEST_ID <- get_request_id(nrow(app_requests))

setwd(ref_loc)
saveRDS(app_requests, "app_requests.rds")


# syncs a set of requests from reference to AWS
sudo_working_key(amazon_keys)
app_requests <- readRDS("app_requests.rds")
existing_files <- list_aws_s3()
request_files <- app_requests$FILE_LOCATION
files_to_be_uploaded <- setdiff(request_files, existing_files)

n <- length(files_to_be_uploaded)

my_range <- seq_len(n)
my_range <- 39401:n
my_range <- 62021:n

for (i in my_range)
{
  if (i %% 100 == 0 || i == n)
  {
    sprintf_clean("Synced %s out of %s", i, n)
  }
  single_file <- files_to_be_uploaded[i]
  data <- load_local(single_file)
  save_aws_s3(data, single_file)
}

