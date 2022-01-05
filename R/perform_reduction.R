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
source_sdr("storage.R")

get_from_dir("amazon_keys", dir = sprintf("%s/dependencies", app_loc))
set_working_key(amazon_keys)

# ----------
# REQUEST ID
# ----------

# gets n request ids
get_request_id <- function(n = 1)
{
  if (n < 1)
    return(numeric())

  id <- 1:n
  if (find_aws_s3("Sessions/cur_id.rds"))
    id <- id + load_aws_s3("Sessions/cur_id.rds")

  save_aws_s3(id[n], "Sessions/cur_id.rds")
  id
}

# -----------------------------
# REQUEST CREATION / VALIDATION
# -----------------------------

# syntactic sugar for !(a %in% b)
`%nin%` <- function(a, b) {
  !(a %in% b)
}

# for integers and numerics: default value is -1
# for author: default value is "ADMIN"
# for all other characters: default value is "-"
num_d <- function(n = 1)
{
  rep(-1, n)
}

aut_d <- function(n = 1)
{
  rep("ADMIN", n)
}

chr_d <- function(n = 1)
{
  rep("-", n)
}

# cleans a request (a requests data.frame with a single row) and returns the cleaned version,
# returning NULL if the request cannot be properly cleaned.
# note: should only be called through make_requests, assuming valid types
# note: if an attribute is unused, we set it to a default value (ex: batch_size to NaN for PCA)
clean_request <- function(request)
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
    return(NULL)

  if (emb == "Sets")
  {
    # Global Min-Max normalization is required
    if (nor != "Global Min-Max")
      return(NULL)

    # needs a valid threshold
    if (!dplyr::between(thr, 0, 1))
      return(NULL)

    # ensure the characteristic is valid
    characteristics <- colnames(select_chars(order_total[[cat]]))
    if (cha %nin% characteristics)
      return(NULL)

    # clean all irrelevant attributes
    request$ROW_SUBSETS <- chr_d()
    request$COL_SUBSETS <- chr_d()
    request$VISUALIZATION <- chr_d()
    request$COMPONENT <- num_d()
    request$DIMENSION <- num_d()
    request$PERPLEXITY <- num_d()
    request$BATCH_SIZE <- num_d()

    return(request)
  }
  else
  {
    # a valid normalization is required
    if (nor %nin% nor_options)
      return(NULL)

    # clean set-related attributes
    request$THRESHOLD <- num_d()
    request$CHARACTERISTIC <- chr_d()

    # must be a valid row subset
    if (row %nin% sub_row_groups[[cat]])
      return(NULL)

    row_num <- categories[[cat]][1]
    if (row != "Total")
      row_num <- length(get_row_decor_subset(cat, row))

    # must be a valid column subset
    if (col %nin% sub_col_groups[[cat]])
      return(NULL)

    col_num <- categories[[cat]][2]
    if (col != "Total")
      col_num <- length(get_col_decor_subset(cat, col))

    # avoid out-of-range perplexities
    max_perplexity <- min(100, floor((row_num - 1)/3))

    # PHATE
    if (emb == "PHATE")
    {
      # must be reduced down to 2 or 3 dimensions for plotting
      if (com != 2 && com != 3)
        return(NULL)
      # avoid out-of-range perplexities
      if (!dplyr::between(per, 0, max_perplexity))
        return(NULL)

      # clean all irrelevant components
      request$VISUALIZATION <- chr_d()
      request$DIMENSION <- num_d()
      request$BATCH_SIZE <- num_d()

      return(request)
    }
    else # PCA, VAE, UMAP
    {
      if (vis %nin% vis_options)
        return(NULL)

      # avoid out-of-range components
      if (!dplyr::between(com, 2, col_num - 1))
        return(NULL)

      if (vis == "tSNE")
      {
        # must go to 2D or 3D and have less columns than first-round reduction
        if ((dim != 2 && dim != 3) || dim >= com)
          return(NULL)

        # avoid out-of-range perplexity
        if (!dplyr::between(per, 0, max_perplexity))
          return(NULL)
      }
      else
      {
        # no second-level reduction? no DIMENSION parameter needed
        request$DIMENSION <- num_d()
      }

      # first round reduction must be plottable in 2D, at least
      if (com < 2)
        return(NULL)

      request$THRESHOLD <- num_d()
      request$CHARACTERISTIC <- chr_d()

      if (emb == "VAE")
      {
        # prevent normalizations that don't end in the range [0, 1]
        if (nor %in% nor_options[4:5])
          return(NULL)

        # avoid out-of-range batch sizes
        if (!dplyr::between(bat, 1, row_num))
          return(NULL)

        if (vis != "tSNE")
          request$PERPLEXITY <- num_d()

        return(request)
      }
      else
      {
        request$BATCH_SIZE <- num_d()

        if (emb == "UMAP")
        {
          # avoid non-integral or negative perplexities
          if (!dplyr::between(per, 0, max_perplexity))
            return(NULL)

          return(request)
        }
        else # emb == "PCA" at this stage
        {
          if (vis != "tSNE")
            request$PERPLEXITY <- num_d()

          return(request)
        }
      }
    }
  }

  return(NULL)
}

# check if we have an integer for an attribute
att_is_int <- function(x)
{
  bool_vec <- suppressWarnings(x == as.integer(x))
  sum(bool_vec) == length(bool_vec)
}

# if the inputs do not correspond to a valid set of requests, return NULL.
# otherwise, return a set of requests in the appropriate form.
# com: components for first-round dimensionality reduction
# dim: dimension for second-round dimensionality reduction
# note: the first 13 attributes MUST be a primary key; author / timestamps don't differentiate
# note: if the TIME_COMPLETED field is before the TIME_REQUESTED field, it hasn't been done yet!
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
  times_requested <- rep(Sys.time(), n_cat)
  requests <- data.frame(
    "CATEGORIES" = cat, "ROW_SUBSETS" = row, "COL_SUBSETS" = col, "SCALING" = sca,
    "NORMALIZATION" = nor, "EMBEDDING" = emb, "VISUALIZATION" = vis, "COMPONENT" = com,
    "DIMENSION" = dim, "PERPLEXITY" = per, "BATCH_SIZE" = bat, "THRESHOLD" = thr,
    "CHARACTERISTIC" = cha, "AUTHOR" = aut,
    "TIME_REQUESTED" = times_requested, "TIME_COMPLETED" = times_requested - 60 * 60 * 24 # minus 1 day
  )

  # cleans all requests, quitting if it's not possible to do so
  for (i in seq_len(nrow(requests)))
  {
    clean_req <- clean_request(requests[i,])
    if (is.null(clean_req))
      return(NULL)
    requests[i,] <- clean_req
  }

  requests
}

# -------------------
# REQUEST FULFILLMENT
# -------------------

# note: only used for PCA, VAE, UMAP (anything else gets the default value)
make_inter_name <- function(cat, row, col, sca, nor, emb, com, per, bat)
{
  sca_ind <- which(sca_options == sca)
  nor_ind <- which(nor_options == nor)

  if (emb == "PCA")
  {
    return(sprintf("PCA/%s/%s_%s_S%s_N%s_%s.rds",
                   cat, row, col, sca_ind, nor_ind, com))
  }

  if (emb == "VAE")
  {
    return(sprintf("VAE/%s/%s_%s_S%s_N%s_%s_%s.rds",
                   cat, row, col, sca_ind, nor_ind, com, bat))
  }

  # UMAP
  if (emb == "UMAP")
  {
    return(sprintf("UMAP/%s/%s_%s_S%s_N%s_%s_%s.rds",
                   cat, row, col, sca_ind, nor_ind, com, per))
  }

  chr_d()
}

# converts requests into the locations of their final files
requests_to_final <- function(requests)
{
  result <- character()

  for (i in seq_len(nrow(requests)))
  {
    args <- as.list(requests[i,])
    names(args) <- NULL
    result <- c(result, do.call(make_sdr_name, args[1:13]))
  }

  result
}

# converts a request into the locations of their intermediate files
requests_to_inter <- function(requests)
{
  result <- character()

  for (i in seq_len(nrow(requests)))
  {
    args <- as.list(requests[i,])
    names(args) <- NULL
    result <- c(result, do.call(make_inter_name, args[c(1:6, 8, 10:11)]))
  }

  result
}

# readRDS but return NULL if force_inter
inter_readRDS <- function(force, int_loc)
{
  if (force != 2 && file.exists(int_loc))
    return(readRDS(int_loc))
  return(NULL)
}

# performs reduction on a group of valid requests,
# parsing requests in a non-sequential order to maximize speed.
# force = 0: if a final-level file already exists, do nothing
# force = 1: override the final-level file
# force = 2: override all intermediate files and the final-level file
# returns the completed version of requests, with TIME_COMPLETED updated.
# note: you can control how a subset of reductions are performed like so:
# subset <- (requests$EMBEDDING == "PCA")
# requests[subset,] <- perform_reduction(requests[subset,], force)
# example:
#     test <- data.frame(matrix(1:25, nrow = 5))
#     subset <- (test$X1 < 4) & (test$X4 %% 2 == 0)
#     test[subset,] <- data.frame(matrix(1:10, nrow = 2))
perform_reduction <- function(requests, force = 0)
{
  # used to make intermediate files easily
  inter_locs <- sprintf("%s/inter/%s", pro_loc, requests_to_inter(requests))
  # used to make final files easily
  final_locs <- sprintf("%s/%s", ref_loc, requests_to_final(requests))
  # used to easily edit completion timestamps
  times_done <- requests$TIME_COMPLETED

  # a true-false vector determining if an analysis should be performed
  i_fin <- !file.exists(final_locs) | rep(force > 0, nrow(requests))

  # select the category
  for (cat in unique(requests$CATEGORIES[i_fin]))
  {
    cat_table <- readRDS(sprintf("%s/combined/combined_%s.rds", pro_loc, cat))
    # if the opened table is not valid, you have a problem
    stopifnot(valid_table(cat_table))

    i_cat <- i_fin & (requests$CATEGORIES == cat)
    # used for sets
    short_order <- select_chars(order_total[[cat]])

    # perform scaling
    for (sca in unique(requests$SCALING[i_cat]))
    {
      sca_table <- do_scal(sca, cat_table)
      i_sca <- i_cat & (requests$SCALING == sca)

      # perform normalization
      for (nor in unique(requests$NORMALIZATION[i_sca]))
      {
        nor_table <- do_norm(nor, sca_table)
        i_nor <- i_sca & (requests$NORMALIZATION == nor)
        i_sets <- i_nor & (requests$EMBEDDING == "Sets")

        # ----
        # SETS
        # ----
        for (thr in unique(requests$THRESHOLD[i_sets]))
        {
          i_thr <- i_sets & (requests$THRESHOLD == thr)
          set_result <- table_to_sets(nor_table, thr)

          for (i in which(i_thr)) # only attributes left: characteristic
          {
            r <- requests[i,]
            f_loc <- final_locs[i]

            sprintf_clean("Generating Sets FINAL: %s", f_loc)
            set_label_matrix(set_result, short_order[[r$CHARACTERISTIC]]) %>% mkdir_saveRDS(f_loc)
            times_done[i] <- Sys.time()
          }
        }

        for (row in unique(requests$ROW_SUBSETS[i_nor]))
        {
          row_table <- get_row_sub(nor_table, cat, row)
          i_row <- i_nor & (requests$ROW_SUBSETS == row)

          for (col in unique(requests$COL_SUBSETS[i_row]))
          {
            col_table <- get_col_sub(row_table, cat, col)
            i_col <- i_row & (requests$COL_SUBSETS == col)

            for (com in unique(requests$COMPONENT[i_col]))
            {
              i_com <- i_col & (requests$COMPONENT == com)
              i_pca <- i_com & (requests$EMBEDDING == "PCA")
              i_vae <- i_com & (requests$EMBEDDING == "VAE")
              i_umap <- i_com & (requests$EMBEDDING == "UMAP")
              i_phate <- i_com & (requests$EMBEDDING == "PHATE")

              # ---
              # PCA
              # ---
              if (sum(i_pca) > 0) # only attributes left: vis [dim] [per]
              {
                # make the intermediate file
                pca_loc <- inter_locs[match(TRUE, i_pca)]
                pca_result <- inter_readRDS(force, pca_loc)
                if (is.null(pca_result))
                {
                  sprintf_clean("Generating PCA INTER: %s", pca_loc)
                  pca_result <- table_to_pca(col_table, com)
                  mkdir_saveRDS(pca_result, pca_loc)
                }

                # make the final files
                for (i in which(i_pca))
                {
                  r <- requests[i,]
                  f_loc <-  final_locs[i]
                  sprintf_clean("Generating PCA FINAL: %s", f_loc)

                  if (r$VISUALIZATION == "Explore")
                    pca_to_explore(pca_result) %>% mkdir_saveRDS(f_loc)

                  if (r$VISUALIZATION == "Summarize")
                    pca_to_summary(pca_result) %>% mkdir_saveRDS(f_loc)

                  if (r$VISUALIZATION == "tSNE")
                    pca_to_tsne(pca_result, r$DIMENSION, r$PERPLEXITY) %>% mkdir_saveRDS(f_loc)

                  times_done[i] <- Sys.time()
                }
              }

              # ---
              # VAE
              # ---
              for (bat in unique(requests$BATCH_SIZE[i_vae]))
              {
                i_bat <- i_vae & (requests$BATCH_SIZE == bat)

                if (sum(i_bat) > 0) # only attributes left: vis [dim] [per]
                {
                  # make the intermediate file
                  vae_loc <- inter_locs[match(TRUE, i_bat)]
                  vae_result <- inter_readRDS(force, vae_loc)
                  if (is.null(vae_result))
                  {
                    sprintf_clean("Generating VAE INTER: %s", vae_loc)
                    vae_result <- table_to_vae(col_table, com, bat)
                    mkdir_saveRDS(vae_result, vae_loc)
                  }

                  # make the final files
                  for (i in which(i_bat))
                  {
                    r <- requests[i,]
                    f_loc <-  final_locs[i]
                    sprintf_clean("Generating VAE FINAL: %s", f_loc)

                    if (r$VISUALIZATION == "Explore")
                      vae_to_explore(vae_result) %>% mkdir_saveRDS(f_loc)

                    if (r$VISUALIZATION == "Summarize")
                      vae_to_summary(vae_result) %>% mkdir_saveRDS(f_loc)

                    if (r$VISUALIZATION == "tSNE")
                      vae_to_tsne(vae_result, r$DIMENSION, r$PERPLEXITY) %>% mkdir_saveRDS(f_loc)

                    times_done[i] <- Sys.time()
                  }
                }
              }

              # ----
              # UMAP
              # ----
              for (per in unique(requests$PERPLEXITY[i_umap]))
              {
                i_per <- i_umap & (requests$PERPLEXITY == per)

                if (sum(i_per) > 0) # only attributes left: [dim]
                {
                  # make the intermediate file
                  umap_loc <- inter_locs[match(TRUE, i_per)]
                  umap_result <- inter_readRDS(force, umap_loc)
                  if (is.null(umap_result))
                  {
                    sprintf_clean("Generating UMAP INTER: %s", umap_loc)
                    umap_result <- table_to_umap(col_table, com, per)
                    mkdir_saveRDS(umap_result, umap_loc)
                  }

                  # make the final files
                  for (i in which(i_per))
                  {
                    r <- requests[i,]
                    f_loc <-  final_locs[i]
                    sprintf_clean("Generating UMAP FINAL: %s", f_loc)

                    if (r$VISUALIZATION == "Explore")
                      umap_to_explore(umap_result) %>% mkdir_saveRDS(f_loc)

                    if (r$VISUALIZATION == "Summarize")
                      umap_to_summary(umap_result) %>% mkdir_saveRDS(f_loc)

                    if (r$VISUALIZATION == "tSNE")
                      umap_to_tsne(umap_result, r$DIMENSION) %>% mkdir_saveRDS(f_loc)

                    times_done[i] <- Sys.time()
                  }
                }
              }

              # -----
              # PHATE
              # -----
              for (i in which(i_phate)) # only attribute left: per
              {
                r <- requests[i,]
                f_loc <-  final_locs[i]
                sprintf_clean("Generating PHATE FINAL: %s", f_loc)

                table_to_phate(col_table, com, r$PERPLEXITY) %>% mkdir_saveRDS(f_loc)
                times_done[i] <- Sys.time()
              }
            }
          }
        }
      }
    }
  }

  requests$TIME_COMPLETED <- times_done
  requests
}

# --------------
# MERGE REQUESTS
# --------------

# removes duplicated requests ... if requests are duplicated,
# (i) favor completed requests and (ii) keep the request
# with the most recent TIME_COMPLETED
# note that order is preserved here
rem_dup_requests <- function(requests)
{
  # the value is (row, TIME_COMPLETED, done) of the request to keep
  index_for_req <- list()
  n <- nrow(requests)

  for (i in seq_len(n))
  {
    r <- requests[i,]
    key <- requests_to_final(r)
    prev_val <- index_for_req[[key]]
    cur_time <- r$TIME_COMPLETED
    cur_done <- cur_time > r$TIME_REQUESTED

    # if no other requests exist,
    # the current request is completed and the previous one is not,
    # or both requests have the same status and the current one is older
    if (is.null(prev_val) ||
        (!prev_val[3] && cur_done) ||
        (prev_val[3] == cur_done && cur_time < prev_val[2]))
      index_for_req[[key]] <- c(i, cur_time, cur_done)
  }

  to_be_kept <- rep(TRUE, n)

  for (i in seq_len(n))
    to_be_kept[i] <- (index_for_req[[requests_to_final(requests[i,])]][1] == i)

  requests[to_be_kept,]
}

