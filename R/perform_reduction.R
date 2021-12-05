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

# requests will be a data.frame with these column names
request_colnames <- c(
  "CATEGORIES",
  "ROW_SUBSETS",
  "COL_SUBSETS",
  "SCALING",
  "NORMALIZATION",
  "EMBEDDING",
  "VISUALIZATION",
  "COMPONENT", # note: components for first-round dimensionality reduction
  "DIMENSION", # note: dimension for second-round dimensionality reduction
  "PERPLEXITY",
  "BATCH_SIZE",
  "THRESHOLD"
)

# returns whether a request (a requests data.frame with a single row) is valid
# note: should only be called through are_valid_requests
# note: NOT YET COMPLETE
is_valid_request <- function(request)
{
  cat <- request$CATEGORIES
  row <- request$ROW_SUBSETS
  col <- request$COL_SUBSETS
  sca <- request$SCALING
  nor <- request$NORMALIZATION
  emb <- request$EMBEDDING
  vis <- request$VISUALIZATION
  com <- request$COMPONENT
  dim <- request$DIMENSION
  per <- request$PERPLEXITY
  bat <- request$BATCH_SIZE
  thr <- request$THRESHOLD

  if (!(cat %in% name_cat))
    return(FALSE)

  if (is.null(get_row_decor_subset(cat, row)))
    return(FALSE)

  if (is.null(get_col_decor_subset(cat, col)))
    return(FALSE)

  if (!(sca %in% sca_options))
    return(FALSE)

  if (!(nor %in% nor_options))
    return(FALSE)

  if (!(emb %in% emb_options))
    return(FALSE)

  return(TRUE);
}

# returns whether a set of requests is valid, by going row-by-row
are_valid_requests <- function(requests)
{
  # ensure it's a data frame
  stopifnot(all.equal(class(data.frame()), class(requests)))

  # ensure its colnames are correct
  stopifnot(all.equal(colnames(requests), request_colnames))

  row_num <- nrow(requests)
  valid_rows <- rep(TRUE, row_num)

  if (row_num > 0)
    for (i in 1:row_num)
      valid_rows[i] <- is_valid_request(requests[i,,drop = FALSE])

  valid_rows
}

# if the inputs do not correspond to a valid set of requests, return NULL.
# otherwise, return a set of requests in the appropriate form.
make_requests <- function(
  cat = character(0),
  row = character(0),
  col = character(0),
  sca = character(0),
  nor = character(0),
  emb = character(0),
  vis = character(0),
  com = numeric(0),
  dim = numeric(0),
  per = numeric(0),
  bat = numeric(0),
  thr = numeric(0)
){
  # it's not a valid request if one set of attributes is longer than another or of the wrong type
  n <- length(cat)
  if (!(
    length(cat) == n && is.character(cat) &&
    length(row) == n && is.character(row) &&
    length(col) == n && is.character(col) &&
    length(sca) == n && is.character(sca) &&
    length(nor) == n && is.character(nor) &&
    length(emb) == n && is.character(emb) &&
    length(vis) == n && is.character(vis) &&
    length(com) == n && is.numeric(com) &&
    length(dim) == n && is.numeric(dim) &&
    length(per) == n && is.numeric(per) &&
    length(bat) == n && is.numeric(bat) &&
    length(thr) == n && is.numeric(thr)
  ))
  return(NULL)

  # create a data frame of requests
  requests <- data.frame(
    "CATEGORIES" = cat,
    "ROW_SUBSETS" = row,
    "COL_SUBSETS" = col,
    "SCALING" = sca,
    "NORMALIZATION" = nor,
    "EMBEDDING" = emb,
    "VISUALIZATION" = vis,
    "COMPONENT" = com,
    "DIMENSION" = dim,
    "PERPLEXITY" = per,
    "BATCH_SIZE" = bat,
    "THRESHOLD" = thr
  )

  if (sum(are_valid_requests(requests)) != n)
    return(NULL)

  requests
}

r1 <- make_requests(
  "miRNA", "Total", "SD_Top_1000", "Logarithmic", "Global Min-Max", "PCA", "Explore", 10, -1, -1, -1, -1)
test_requests <- rbind(test_requests, )
test_requests <- rbind(test_requests, make_request(
  "miRNA", "Plasma", "SD_Top_1000", "Logarithmic", "Global Min-Max", "PCA", "Explore", 10, -1, -1, -1, -1))
test_requests <- rbind(test_requests, make_request(
  "miRNA", "Total", "SD_Top_100", "Logarithmic", "Global Min-Max", "PCA", "Explore", 10, -1, -1, -1, -1))
test_requests <- rbind(test_requests, make_request(
  "miRNA", "Plasma", "SD_Top_100", "Logarithmic", "Global Min-Max", "PCA", "Explore", 10, -1, -1, -1, -1))
perform_reduction(test_requests)

# note: valid requests have the following characteristics:
# if emb == "Sets": row == "Total", col == "Total", nor == "Global Min-Max"
# if vis == "tSNE": require perplexity
# if emb == "VAE": require nor to be one of the 3 VAE-ok options
# set a hard limit on perplexity
# force = 0: if a file already exists, do nothing
# force = 1: override the final-level file
# force = 2: override all intermediate files and the final-level file
# note: to do requests in steps, just subset the requests data.frame
perform_reduction <- function(requests, force = 0)
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
  # table_to_sets(data, thre), set_label_matrix(sets_result, labels)
}

table_name <- paste(test_requests[4, 1:5], collapse = "_")
pca_100_plasma <- readRDS(sprintf("inter/%s_%s_%s.rds", table_name, "PCA", 10))
subset_labels <- order_total$miRNA[rownames(combined_miRNA) %in% get_row_decor_subset("miRNA", "Plasma"),,drop=FALSE]
plotly_2d(pca_100_plasma$x[,1], pca_100_plasma$x[,2], subset_labels$BIOFLUID)
plotly_2d(pca_100_plasma$x[,1], pca_100_plasma$x[,2], subset_labels$CONDITION)
