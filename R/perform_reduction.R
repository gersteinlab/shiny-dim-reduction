# The purpose of this file is to perform a set of requested reduction analyses.
# Intermediate-level files will be saved in pro_loc/inter
# Final-level files will be saved in ref_loc

if (!exists("ran_install"))
{
  if (file.exists("install.R"))
    source("install.R")
  else
    stop("Could not confirm installation. Please source install.R manually.")
}

source_sdr("red_methods.R")
source_sdr("preprocess.R")

# ---------
# FUNCTIONS
# ---------

# syntactic sugar for !(a %in% b)
`%nin%` <- function(a, a) {
  !(a %in% b)
}

# returns whether a request (a requests data.frame with a single row) is valid
# note: should only be called through make_requests, assuming valid types
# note: if an attribute is unused, we don't check it (ex: batch_size for pca)
is_valid_request <- function(request)
{
  cat <- request$CATEGORIES
  sca <- request$SCALING
  nor <- request$NORMALIZATION
  emb <- request$EMBEDDING
  row <- request$ROW_SUBSETS
  col <- request$COL_SUBSETS
  vis <- request$VISUALIZATION
  com <- request$COMPONENT
  dim <- request$DIMENSION
  per <- request$PERPLEXITY
  bat <- request$BATCH_SIZE
  thr <- request$THRESHOLD
  cha <- request$CHARACTERISTIC

  if (cat %nin% name_cat || emb %nin% emb_options || sca %nin% sca_options)
    return(FALSE)

  # Sets doesn't care about row, col
  if (emb == "Sets")
  {
    # needs a valid threshold
    if (!dplyr::between(thr, 0, 1))
      return(FALSE)

    # only Global Min-Max normalization allowed
    if (nor != "Global Min-Max")
      return(FALSE)

    # ensure the characteristic is valid
    characteristics <- colnames(select_chars(order_total[[cat]]))
    if (cha %nin% characteristics)
      return(FALSE)
  }
  else
  {
    # must be a valid row subset
    if (row %nin% sub_row_groups[[cat]])
      return(FALSE)

    row_num <- categories[[cat]][1]
    if (row != "Total")
      row_num <- length(get_row_decor_subset(cat, row))

    # must be a valid column subset
    if (col %nin% sub_col_groups[[cat]])
      return(FALSE)

    col_num <- categories[[cat]][2]
    if (col != "Total")
      col_num <- length(get_col_decor_subset(cat, col))

    # avoid non-integral or out-of-range perplexities
    max_perplexity <- min(100, floor((row_num - 1)/3))

    # PHATE
    if (emb == "PHATE")
    {
      if (nor %nin% nor_options)
        return(FALSE)
      # must be reduced down to 2 or 3 dimensions for plotting
      if (com != 2 && com != 3)
        return(FALSE)
      # avoid out-of-range perplexities
      if (!dplyr::between(per, 0, max_perplexity))
        return(FALSE)
    }
    else # PCA, VAE, UMAP
    {
      if (vis %nin% vis_options)
        return(FALSE)

      # avoid out-of-range components
      if (!dplyr::between(com, 2, col_num - 1))
        return(FALSE)

      if (vis == "tSNE")
      {
        # must go to 2D or 3D and have less columns than first-round reduction
        if ((dim != 2 && dim != 3) || dim >= com)
          return(FALSE)

        # avoid out-of-range perplexity
        if (!dplyr::between(per, 0, max_perplexity))
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

        # avoid out-of-range batch sizes
        if (!dplyr::between(bat, 1, row_num))
          return(FALSE)
      }

      if (nor %nin% nor_options)
        return(FALSE)

      if (emb == "UMAP")
      {
        # avoid non-integral or negative perplexities
        if (!dplyr::between(per, 0, max_perplexity))
          return(FALSE)
      }
    }
  }

  TRUE
}

# check if we have an integer for an attribute
att_is_int <- function(x)
{
  bool_vec <- suppressWarnings(x == as.integer(x))
  sum(bool_vec) == length(bool_vec)
}

# makes it easy for admin to sign requests
aut_n <- function(n)
{
  rep("ADMIN", n)
}

# if the inputs do not correspond to a valid set of requests, return NULL.
# otherwise, return a set of requests in the appropriate form.
# com: components for first-round dimensionality reduction
# dim: dimension for second-round dimensionality reduction
# thr: all thresholds are rounded to 3 digits past the decimal point
make_requests <- function(
  cat = character(), row = character(), col = character(), sca = character(),
  nor = character(), emb = character(), vis = character(), com = numeric(),
  dim = numeric(), per = numeric(), bat = numeric(), thr = numeric(),
  cha = character(), aut = character()
){
  # it's not a valid request if the lengths of all attributes aren't equal
  n_cat <- length(cat)
  att_lengths <- c(
    n_cat, length(row), length(col), length(sca), length(nor), length(emb), length(vis),
    length(com), length(dim), length(per), length(bat), length(thr), length(cha), length(aut)
  )
  if (length(unique(att_lengths)) != 1)
    return(NULL)

  # ensure types are correct
  if (!(
    is.character(cat) && is.character(row) && is.character(col) && is.character(sca) &&
    is.character(nor) && is.character(emb) && is.character(vis) && att_is_int(com) &&
    att_is_int(dim) && att_is_int(per) && att_is_int(bat) && is.numeric(thr) &&
    is.character(cha) && is.character(aut)
  ))
    return(NULL)

  # create a data frame of requests
  requests <- data.frame(
    "CATEGORIES" = cat, "ROW_SUBSETS" = row, "COL_SUBSETS" = col, "SCALING" = sca,
    "NORMALIZATION" = nor, "EMBEDDING" = emb, "VISUALIZATION" = vis, "COMPONENT" = com,
    "DIMENSION" = dim, "PERPLEXITY" = per, "BATCH_SIZE" = bat, "THRESHOLD" = round(thr, 3)
    "CHARACTERISTIC" = cha, "AUTHOR" = aut,
    "TIME_REQUESTED" = rep(Sys.time(), n_cat), "TIME_COMPLETED" = rep(NULL, n_cat)
  )

  # check if every single request is valid
  for (i in seq_along(numeric(nrow(requests))))
    if (!is_valid_request(requests[i,]))
      return(NULL)

  requests
}


filename_from_request <- function(request)
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
  cha <- request$CHARACTERISTIC

  if (emb == "Sets")
  {
    return(sprintf("Sets/Sets_%s_%s_%s_%s.rds", cat, sca, thr, cha))
  }

  if (emb == "PCA")
  {

  }
}

# note: valid requests have the following characteristics:
# force = 0: if a final-level file already exists, do nothing
# force = 1: override the final-level file
# force = 2: override all intermediate files and the final-level file
# note: to do requests in steps, just subset the requests data.frame
# returns a
perform_reduction <- function(requests, force = 0)
{
  # select the category
  for (cat in unique(requests$CATEGORIES))
  {
    # generally, the biggest source of error is if the opened table is not valid
    cat_table <- readRDS(sprintf("combined/combined_%s.rds", cat))
    stopifnot(valid_table(cat_table))

    subrequests_cat <- requests[requests$CATEGORIES == cat,]
    short_order <- select_chars(order_total[[cat]])

    # perform scaling
    for (sca in unique(subrequests_cat$SCALING))
    {
      sca_table <- do_scal(sca, cat_table)
      subrequests_sca <- subrequests_cat[subrequests_cat$SCALING == sca,]

      # perform normalization
      for (nor in unique(subrequests_sca$NORMALIZATION))
      {
        subrequests_nor <- subrequests_sca[subrequests_sca$NORMALIZATION == nor,]
        nor_table <- do_norm(nor, sca_table)

        # sets is unique; solve it first
        are_set_requests <- (subrequests_nor$EMBEDDING == "Sets")
        subrequests_sets <- subrequests_nor[are_set_requests,]

        # perform Sets
        for (thr in unique(subrequests_sets$THRESHOLD))
        {
          set_result <- table_to_sets(nor_table, thr)
          subrequests_thr <- subrequests_sets[subrequests_nor$THRESHOLD == thr]

          for (request in subrequests_thr)
          {
            set_loc <- sprintf("Sets/Sets-%s_%s_%s_%s.rds",
                               ind, sca_ind, cha, cat)

            if (force > 0 || !file.exists(set_loc))
              saveRDS(set_label_matrix(set_result, short_order), set_loc)
          }
        }

        # then move to all other embeddings, which require row / col subsets
        subrequests_not_sets <- requests[!are_set_requests, ]

        for (row in unique(subrequests_not_sets$ROW_SUBSETS))
        {
          subrequests_row <- subrequests_not_sets[subrequests_not_sets$ROW_SUBSETS == row,]
          row_table <- get_row_sub(nor_table, cat, row)

          for (col in unique(subrequests_row$COL_SUBSETS))
          {
            subrequests_col <- subrequests_row[subrequests_row$COL_SUBSETS == col,]
            col_table <- get_col_sub(row_table, cat, col)

            for (emb in unique(subrequests_nor$EMBEDDING))
            {
              subrequests_emb <- subrequests_col[subrequests_col$EMBEDDING == emb,]

              # print(paste(cat, row, col, sca, nor, emb, sep = ", "))

              if (emb == "PCA")
              {
                for (com in unique(subrequests_emb$COMPONENT))
                {
                  pca_loc <- sprintf("inter/%s_%s_%s.rds", table_name, emb, com)
                  pca_result <- NULL
                  if (force == 2 || !file.exists(pca_loc))
                  {
                    pca_result <- table_to_pca(nor_table, com)
                    saveRDS(pca_result, pca_loc)
                  }
                  else
                  {
                    pca_result <- readRDS(pca_loc)
                  }

                  subrequests_com <- subrequests_emb[subrequests_emb$COMPONENT == com,]

                  for (request in subrequests_com)
                  {
                    if (request$VISUALIZATION == "Explore")
                    {
                      saveRDS(pca_to_explore(pca_result),
                    }
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
