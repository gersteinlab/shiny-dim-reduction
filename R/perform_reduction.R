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

# check if we have an integer for an attribute
att_is_int <- function(x)
{
  suppressWarnings(isTRUE(x == as.integer(x)))
}

# returns whether a request (a requests data.frame with a single row) is valid
# note: should only be called through are_valid_requests
# note: if an attribute is unused, we don't check it (ex: batch_size for pca)
# note: a request can be valid but still impossible to execute (perplexity too high)
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

  if (!is.character(cat) || !(cat %in% name_cat))
    return(FALSE)

  if (!is.character(emb) || !(emb %in% emb_options))
    return(FALSE)

  if (!is.character(sca) || !(sca %in% sca_options))
    return(FALSE)

  # Sets doesn't care about row, col
  if (emb == "Sets")
  {
    # needs a valid threshold
    if (!is.numeric(thr) || !dplyr::between(thr, 0, 1))
      return(FALSE)

    # only Global Min-Max normalization allowed
    if (nor != "Global Min-Max")
      return(FALSE)
  }
  else
  {
    # must be a valid row subset
    if (!is.character(row) || !(row %in% sub_row_groups[[cat]]))
      return(FALSE)

    row_num <- categories[[cat]][1]
    if (row != "Total")
      row_num <- length(get_row_decor_subset(cat, row))

    # must be a valid column subset
    if (!is.character(col) || !(col %in% sub_col_groups[[cat]]))
      return(FALSE)

    col_num <- categories[[cat]][2]
    if (col != "Total")
      col_num <- length(get_col_decor_subset(cat, col))

    # avoid non-integral or out-of-range perplexities
    max_perplexity <- min(100, floor((row_num - 1)/3))

    # PHATE
    if (emb == "PHATE")
    {
      if (!is.character(nor) || !(nor %in% nor_options))
        return(FALSE)
      # must be reduced down to 2 or 3 dimensions for plotting
      if (com != 2 && com != 3)
        return(FALSE)
      # avoid non-integral or out-of-range perplexities
      if (!att_is_int(per) || !dplyr::between(per, 0, max_perplexity))
        return(FALSE)
    }
    else # PCA, VAE, UMAP
    {
      if (!is.character(vis) || !(vis %in% vis_options))
        return(FALSE)

      # avoid non-integral or out-of-range components
      if (!att_is_int(com) || !dplyr::between(com, 2, col_num - 1))
        return(FALSE)

      if (vis == "tSNE")
      {
        # must go to 2D or 3D and have less columns than first-round reduction
        if ((dim != 2 && dim != 3) || dim >= com)
          return(FALSE)

        if (!att_is_int(per) || !dplyr::between(per, 0, max_perplexity))
          return(FALSE)
      }

      # first round reduction must be plottable in 2D, at least
      if (com < 2)
        return(FALSE)

      if (emb == "VAE")
      {
        # prevent normalizations that don't end in [0, 1]
        if (nor %in% nor_options[4:5])
          return(FALSE)

        # avoid non-integral or out-of-range batch sizes
        if (!att_is_int(bat) || !dplyr::between(bat, 1, row_num))
          return(FALSE)
      }

      if (!is.character(nor) || !(nor %in% nor_options))
        return(FALSE)

      if (emb == "UMAP")
      {
        # avoid non-integral or negative perplexities
        if (!att_is_int(per) || !dplyr::between(per, 0, max_perplexity))
          return(FALSE)
      }
    }
  }

  TRUE
}

# returns whether a set of requests is valid, by going row-by-row
are_valid_requests <- function(requests)
{
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
  cat = character(0), row = character(0), col = character(0),
  sca = character(0), nor = character(0), emb = character(0),
  vis = character(0), com = numeric(0), dim = numeric(0),
  per = numeric(0), bat = numeric(0), thr = numeric(0)
){
  # it's not a valid request if the lengths of all attributes aren't equal
  n_cat <- length(cat)
  att_lengths <- c(
    n_cat, length(row), length(col), length(sca), length(nor), length(emb),
    length(vis), length(com), length(dim), length(per), length(bat), length(thr)
  )
  if (length(unique(att_lengths)) != 1)
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
    "THRESHOLD" = thr # note that only 3 digits past the decimal point count
  )

  if (is.numeric(thr))
    requests$THRESHOLD <- round(thr, 3)

  # check if every single request is valid
  if (sum(are_valid_requests(requests)) != n_cat)
    return(NULL)

  requests
}

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
  # select the category
  for (cat in unique(requests$CATEGORIES))
  {
    subrequests_cat <- requests[requests$CATEGORIES == cat,]
    cat_table <- readRDS(sprintf("combined/combined_%s.rds", cat))
    short_order <- select_chars(order_total[[cat]])

    # perform scaling
    for (sca in unique(subrequests_cat$SCALING))
    {
      subrequests_sca <- subrequests_cat[subrequests_cat$SCALING == sca,]
      sca_table <- do_scal(sca, cat_table)

      # perform normalization
      for (nor in unique(subrequests_sca$NORMALIZATION))
      {
        subrequests_nor <- subrequests_sca[subrequests_sca$NORMALIZATION == nor,]
        nor_table <- do_norm(nor, sca_table)

        # divide into embeddings
        subrequests_sets <- requests[requests$EMBEDDING == "Sets",]

        # perform Sets
        for (thr in unique(subrequests_sets$THRESHOLD))
        {
          set_result <- table_to_sets(nor_table, thr)

          for (cha in colnames(short_order))
          {
            set_loc <- sprintf("Sets/Sets-%s_%s_%s_%s.rds",
                              ind, sca_ind, cha, cat)

            if (force > 0 || !file.exists(set_loc))
              saveRDS(set_label_matrix(set_result, short_order[[cha]]), set_loc)
          }
        }

        subrequests_pca <- requests[requests$EMBEDDING == "PCA", ]
        subrequests_vae <- requests[requests$EMBEDDING == "VAE", ]
        subrequests_umap <- requests[requests$EMBEDDING == "UMAP", ]
        subrequests_phate <- requests[requests$EMBEDDING == "PHATE", ]

        for (row in unique(subrequests_cat$ROW_SUBSETS))
        {
          subrequests_row <- subrequests_cat[subrequests_cat$ROW_SUBSETS == row,]

          # Sets doesn't care about row / col subsetting, so treat it like a custom case
          if (unique(subrequests_row$EMBEDDING) == "Sets")
          {

          }

          row_table <- get_row_sub(cat_table, cat, row)

          for (col in unique(subrequests_row$COL_SUBSETS))
          {
            subrequests_col <- subrequests_row[subrequests_row$COL_SUBSETS == col,]
            col_table <- get_col_sub(row_table, cat, col)



            stopifnot(valid_table(nor_table))

            table_name <- paste(cat, row, col, sca, nor, sep = ", ")
            print(table_name)
            print(dim(nor_table))

            for (emb in unique(subrequests_nor$EMBEDDING))
            {
              subrequests_emb <- subrequests_sca[subrequests_sca$EMBEDDING == emb,]

              if (emb == "PCA")
              {
                for (dim in unique(subrequests_emb$DIMENSION))
                {
                  pca_loc <- sprintf("inter/%s_%s_%s.rds", table_name, emb, dim)
                  pca_result <- NULL
                  if (force == 2 || !file.exists(explore_loc))
                  {
                    pca_result <- table_to_pca(nor_table, dim)
                    saveRDS(explore, explore_loc)
                  }
                  else
                  {
                    pca_result <- readRDS(pca_loc)
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

  # table_to_pca(table, dim = 2), pca_to_summary(pca_result)
  # table_to_vae(table, dim = 2, batch_size = 2), vae_to_summary(vae_result)
  # table_to_umap(table, dim = 2, perp = 2), umap_to_summary(umap_result)
  # table_to_phate(data, dim = 2, perp = 1), table_to_tsne(table, dim = 2, perp = 1)

  # table_to_sets(data, thre), set_label_matrix(sets_result, labels)
}
