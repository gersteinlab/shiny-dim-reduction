# The purpose of this file is to perform a set of requested reduction analyses.

if (!exists("ran_install"))
{
  if (file.exists("install.R"))
    source("install.R")
  else
    stop("Could not confirm installation. Please source install.R manually.")
}

source_sdr("red_methods.R")
source_sdr("preprocess.R")

make_request <- function(
  cat = character(0),
  row = character(0),
  col = character(0),
  sca = character(0),
  nor = character(0),
  emb = character(0),
  vis = character(0),
  dim = numeric(0),
  per = numeric(0),
  bat = numeric(0),
  knn = numeric(0),
  thr = numeric(0)
)
{
  all_lengths <- c(
    length(cat), length(row), length(col), length(sca), length(nor), length(emb),
    length(vis), length(dim), length(per), length(bat), length(knn), length(thr)
  )
  stopifnot(length(unique(all_lengths))  == 1)

  data.frame(
    "CATEGORIES" = cat,
    "ROW_SUBSETS" = row,
    "COL_SUBSETS" = col,
    "SCALING" = sca,
    "NORMALIZATION" = nor,
    "EMBEDDING" = emb,
    "VISUALIZATION" = vis,
    "DIMENSION" = dim,
    "PERPLEXITY" = per,
    "BATCH_SIZE" = bat,
    "KNN" = knn,
    "THRESHOLD" = thr
  )
}

test_requests <- make_request()
test_requests <- rbind(test_requests, make_request(
  "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "PCA", "Explore", 10, -1, -1, -1, -1))
test_requests <- rbind(test_requests, make_request(
  "miRNA", "Plasma", "SD_Top_1000", "Logarithmic", "Global Min-Max", "PCA", "Explore", 10, -1, -1, -1, -1))
test_requests <- rbind(test_requests, make_request(
  "miRNA", "Total", "SD_Top_100", "Logarithmic", "Global Min-Max", "PCA", "Explore", 10, -1, -1, -1, -1))
test_requests <- rbind(test_requests, make_request(
  "miRNA", "Plasma", "SD_Top_100", "Logarithmic", "Global Min-Max", "PCA", "Explore", 10, -1, -1, -1, -1))
perform_reduction(test_requests)

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
# force = 0: if a file already exists, do nothing
# force = 1: override the final-level file
# force = 2: override all intermediate files
perform_reduction <- function(requests, max_analyses = 100, force = 0)
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
        col_table <- get_col_sub(row_table, cat, col)

        for (sca in unique(subrequests_col$SCALING))
        {
          subrequests_sca <- subrequests_col[subrequests_col$SCALING == sca,]
          sca_table <- do_scal(sca, col_table)

          for (nor in unique(subrequests_sca$NORMALIZATION))
          {
            subrequests_nor <- subrequests_sca[subrequests_sca$NORMALIZATION == nor,]
            nor_table <- do_norm(nor, sca_table)

            stopifnot(valid_table(nor_table))

            table_name <- paste(cat, row, col, sca, nor, sep = "_")
            print(table_name)
            print(dim(nor_table))

            for (emb in unique(subrequests_nor$EMBEDDING))
            {
              subrequests_emb <- subrequests_sca[subrequests_sca$EMBEDDING == emb,]

              if (emb == "PCA")
              {
                for (dim in unique(subrequests_emb$DIMENSION))
                {
                  explore_loc <- sprintf("inter/%s_%s_%s.rds", table_name, emb, dim)
                  explore <- NULL
                  if (!file.exists(explore_loc))
                  {
                    explore <- table_to_pca(nor_table, dim)
                    saveRDS(explore, explore_loc)
                  }
                  else
                  {
                    explore <- readRDS(explore_loc)
                  }
                }
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
