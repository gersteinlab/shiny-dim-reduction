# This file tests preprocess.R by inspecting reduction parameters, generating analysis names,
# and testing whether reading / writing to those file names actually works.

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

source_sdr("preprocess.R")

row_data <- empty_named_list(name_cat)
col_data <- empty_named_list(name_cat)

indices_to_logi <- function(indices, n)
{
  seq_len(n) %in% indices
}

subsets_num_to_logi <- function(subsets, n)
{
  for (sub in names(subsets))
    subsets[[sub]] <- indices_to_logi(subsets[[sub]], n)
  subsets
}

for (cat in name_cat)
{
  row_len <- as.integer(categories[[cat]][1])
  row_subs <- decorations[[cat]]$ROW_SUBSETS

  if (cat %in% c("rRNA_Transpose", "Gene_Transpose"))
    row_names <- order_total[[cat]]$species
  else
  {
    if (cat %in% c("RNA_binding_proteins", "DGCR8_HepG2", "DGCR8_HepG2+K562",
                   "DGCR8_K562", "DROSHA_HepG2", "DROSHA_HepG2+K562", "DROSHA_K562"))
      row_names <- order_total[[cat]]$DOWNLOAD_NAME
    else
      row_names <- order_total[[cat]]$FASTQ_IDENTIFIER
  }
  row_data_cat <- list(
    "length" = row_len,
    "names" = row_names,
    "subsets" = subsets_num_to_logi(row_subs, row_len)
  )
  stopifnot(is_axis(row_data_cat))
  row_data[[cat]] <- row_data_cat

  col_len <- as.integer(categories[[cat]][2])
  col_subs <- decorations[[cat]]$COL_SUBSETS
  col_names <- col_subs$Reference
  col_subs$Reference <- NULL
  col_data_cat <- list(
    "length" = col_len,
    "names" = col_names,
    "subsets" = subsets_num_to_logi(col_subs, col_len)
  )
  stopifnot(is_axis(col_data_cat))
  col_data[[cat]] <- col_data_cat
}
