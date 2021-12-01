# The purpose of this file is to perform a set of requested reduction analyses.

if (!exists("ran_install"))
{
  if (file.exists("install.R"))
    source("install.R")
  else
    stop("Could not confirm installation. Please source install.R manually.")
}

source_sdr("preprocess.R")
source_sdr("red_methods.R")

empty_request <- function()
{
  data.frame(
    "CATEGORIES" = character(0),
    "ROW_SUBSETS" = character(0),
    "COL_SUBSETS" = character(0),
    "SCALING" = character(0),
    "NORMALIZATION" = character(0),
    "EMBEDDING" = character(0),
    "VISUALIZATION" = character(0),
    "DIMENSION" = numeric(0),
    "PERPLEXITY" = numeric(0),
    "BATCH_SIZE" = numeric(0),
    "KNN" = numeric(0),
    "CUTOFF" = numeric(0)
  )
}

valid_request <- function(request)
{

}

prune_requests <- function(requests)
{
  # ensure it's a data frame
  stopifnot(all.equal(class(data.frame()), class(requests)))

  row_num <- nrow(requests)
  if (row_num < 1)
    return(TRUE)

  valid_rows <- rep(TRUE, row_num)

  for (i in 1:row_num)
    valid_rows[i] <- valid_request(requests[i,,drop = FALSE])

  requests[valid_rows,,drop = FALSE]
}


# note: valid requests have the following characteristics:
# if emb == "Sets": row == "Total", col == "Total", nor == "Global Min-Max"
# if vis == "tSNE": require perplexity
# if emb == "VAE": require nor to be one of the 3 VAE-ok options
# set a hard limit on perplexity
perform_reduction <- function(requests, max_analyses = 100)
{
  for (cat in unique(requests$CATEGORIES))
  {
    subrequests_cat <- requests[requests$CATEGORIES == cat,]
    cat_table <- readRDS(sprintf("combined/combined_%s.rds", cat))

    for (row in unique(subrequests_cat$ROW_SUBSETS))
    {
      subrequests_row <- subrequests_cat[subrequests_cat$ROW_SUBSETS == row,]
      row_table <- get_row_sub(cat_table, cat, row)

      for (col in unique(subrequests_row$COL_SUBSETS))
      {
        subrequests_col <- subrequests_row[subrequests_row$COL_SUBSETS == col,]
        col_table <- get_col_sub(row_sub_table, cat, col)

        for (sca in unique(subrequests_col$SCALING))
        {
          subrequests_sca <- subrequests_col[subrequests_col$SCALING == sca,]
          sca_table <- do_scal(sca, col_table)

          for (nor in unique(subrequests_sca$NORMALIZATION))
          {
            subrequests_nor <- subrequests_sca[subrequests_sca$NORMALIZATION == nor,]
            nor_table <- do_norm(nor, sca_table)

            stopifnot(valid_table(nor_table))

            for (emb in unique(subrequests_sca$EMBEDDING))
            {
              if (emb == "PCA")
              {

              }

              if (emb == "VAE")
              {

              }

              if (emb == "UMAP")
              {

              }

              if (emb == "PHATE")
              {

              }

              if (emb == "Sets")
              {

              }
            }
          }
        }
      }
    }
  }

  # table_to_pca(table, dim = 2)
  # pca_to_summary(pca_result)
  # table_to_vae(table, dim = 2, batch_size = 2)

  # vae_to_summary(vae_result)
  # table_to_umap(table, dim = 2, perp = 2)
  # umap_to_summary(umap_result)
  # table_to_phate(data, dim = 2, perp = 1)
  # table_to_tsne(table, dim = 2, perp = 1)
  # table_to_sets(data, cutoff), set_label_matrix(sets_result, labels)
}
