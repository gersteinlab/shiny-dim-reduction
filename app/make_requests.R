# The purpose of this file is to store functions for validating,
# creating, and combining reduction requests.

if (!exists("sdr_config"))
  source("app/install.R")

source_app("preprocess.R")

# ----------------
# ANALYSIS OPTIONS
# ----------------

# scale options
sca_options <- c(
  "Logarithmic",
  "Linear"
)

get_sca_ind <- function(sca)
{
  which(sca_options == sca)
}

# normalization options
nor_options <- c(
  "Global Min-Max",
  "Local Min-Max",
  "Global Z-Score",
  "Local Z-Score",
  "Quantile"
)

get_nor_ind <- function(nor)
{
  which(nor_options == nor)
}

# embedding options
emb_options <- c(
  "PCA",
  "VAE",
  "UMAP",
  "PHATE",
  "Sets"
)

# visualization options
vis_options <- c(
  "Explore",
  "Summarize",
  "tSNE"
)

# ----------------------
# REQUEST KEY VALIDATION
# ----------------------

# ANATOMY OF A REQUEST:
# first 13 columns constitute a request key (req_key),
# which determines the analysis performed, the name of
# all intermediate / final reductions
req_key_members <- c(
  # see install.R
  "CATEGORIES" = "cat",
  # see preprocess.R
  "ROW_SUBSETS" = "row",
  # see preprocess.R
  "COL_SUBSETS" = "col",
  # see sca_options
  "SCALING" = "sca",
  # see nor_options
  "NORMALIZATION" = "nor",
  # see red_methods.R
  "EMBEDDING" = "emb",
  # see red_methods.R
  "VISUALIZATION" = "vis",
  # com: components for first-round dimensionality reduction
  # note: should be integer
  "COMPONENT" = "com",
  # dim: dimension for second-round dimensionality reduction
  # note: should be integer
  "DIMENSION" = "dim",
  # see red_methods.R
  # note: should be integer
  "PERPLEXITY" = "per",
  # see red_methods.R
  # note: should be integer
  "BATCH_SIZE" = "bat",
  # see red_methods.R
  "THRESHOLD" = "thr",
  # see red_methods.R
  "CHARACTERISTIC" = "cha"
)


num_d <- -1 # default numeric value
aut_d <- "ADMIN" # default author value
chr_d <- "-" # default character value

# are these request keys?
are_req_keys <- function(x)
{
  is.data.frame(x) && identical(names(x), names(req_key_members)) &&
    is.character(x$CATEGORIES) &&
    is.character(x$ROW_SUBSETS) &&
    is.character(x$COL_SUBSETS) &&
    is.character(x$SCALING) &&
    is.character(x$NORMALIZATION) &&
    is.character(x$EMBEDDING) &&
    is.character(x$VISUALIZATION) &&
    is.numeric(x$COMPONENT) &&
    is.numeric(x$DIMENSION) &&
    is.numeric(x$PERPLEXITY) &&
    is.numeric(x$THRESHOLD) &&
    is.character(x$CHARACTERISTIC)
}

# cleans request keys, stopping if any error is encountered
# note: should only be called through make_req_keys, assuming valid types
# note: if an attribute is unused, we set it to the default value.
clean_req_keys <- function(req_keys)
{
  # abbreviations
  cat_vec <- req_keys$CATEGORIES
  row_vec <- req_keys$ROW_SUBSETS
  col_vec <- req_keys$COL_SUBSETS
  sca_vec <- req_keys$SCALING
  nor_vec <- req_keys$NORMALIZATION
  emb_vec <- req_keys$EMBEDDING
  vis_vec <- req_keys$VISUALIZATION
  com_vec <- req_keys$COMPONENT
  dim_vec <- req_keys$DIMENSION
  per_vec <- req_keys$PERPLEXITY
  bat_vec <- req_keys$BATCH_SIZE
  thr_vec <- req_keys$THRESHOLD
  cha_vec <- req_keys$CHARACTERISTIC

  stopifnot(
    cat_vec %in% cat_names,
    emb_vec %in% emb_options,
    sca_vec %in% sca_options,
    nor_vec %in% nor_options
  )

  # --------
  # CLEANING
  # --------

  # break up by embedding
  is_pca <- (emb_vec == "PCA")
  is_vae <- (emb_vec == "VAE")
  is_umap <- (emb_vec == "UMAP")
  is_phate <- (emb_vec == "PHATE")
  is_sets <- (emb_vec == "Sets")
  # (P)CA, (V)AE, (U)MAP
  is_pvu <- is_pca & is_vae & is_umap

  # set all unnecessary attributes to default values
  req_keys$ROW_SUBSETS[is_sets] <- chr_d
  req_keys$COL_SUBSETS[is_sets] <- chr_d
  req_keys$VISUALIZATION[is_sets | is_phate] <- chr_d
  req_keys$COMPONENT[is_sets] <- num_d
  req_keys$BATCH_SIZE[!is_vae] <- num_d
  req_keys$THRESHOLD[!is_sets] <- num_d
  req_keys$CHARACTERISTIC[!is_sets] <- chr_d

  # do the same for the values that depend on earlier cleaning
  is_tsne <- (vis_vec == "tSNE")
  uses_per <- (is_umap | is_phate | is_tsne)
  req_keys$DIMENSION[is_sets | is_phate | !is_tsne] <- num_d
  req_keys$PERPLEXITY[!uses_per] <- num_d

  # ----------
  # VALIDATION
  # ----------

  stopifnot(
    # for Sets, Global Min-Max normalization is required
    nor_vec[is_sets] == "Global Min-Max",
    # for Sets, thresholds between 0 and 1 are required
    vec_between(thr_vec[is_sets], 0, 1),
    # for VAE, require normalizations that stay in [0, 1]
    nor_vec[is_vae] %in% c("Global Min-Max", "Local Min-Max"),
    # for PVU, visualization options must be valid
    vis_vec[is_pvu] %in% vis_options,
    # for PHATE, the end result must be 2 or 3 dimensions
    com_vec[is_phate] %in% c(2, 3),
    # for tSNE, the end result must be 2 or 3 dimensions
    dim_vec[is_tsne] %in% c(2, 3)
  )

  # for Sets, characteristics must be in relevant metadata cols
  for (cat in cat_names)
  {
    rel_meta <- get_row_axis(cat)$rel_meta
    stopifnot(cha_vec[is_sets & (cat_vec == cat)] %in% rel_meta)
  }

  # get the number of rows / columns for various subsets
  row_n_vec <- rep(NA, nrow(req_keys))
  col_n_vec <- rep(NA, nrow(req_keys))

  for (cat in cat_names)
  {
    is_cat <- (cat_vec == cat)

    row_sub_lens <- get_row_subset_lengths(cat)
    for (row in names(row_sub_lens))
      row_n_vec[is_cat & (row_vec == row)] <- row_sub_lens[[row]]

    col_sub_lens <- get_col_subset_lengths(cat)
    for (col in names(col_sub_lens))
      col_n_vec[is_cat & (col_vec == col)] <- col_sub_lens[[col]]
  }

  # enforce valid ranges
  stopifnot(
    perplexity_is_valid(per_vec[uses_per], row_n_vec[uses_per]),
    # for PVU tSNE, components must be between dim, col_n
    vec_between(
      com_vec[is_tsne],
      dim_vec[is_tsne],
      col_n_vec[is_tsne]
    ),
    # for PVU non-tSNE, components must be between 2, col_n
    vec_between(
      com_vec[is_pvu & !is_tsne],
      2L,
      col_n_vec[is_pvu & !is_tsne]
    ),
    # batch sizes must be between 1, row_n
    vec_between(
      bat_vec[is_vae],
      1L,
      row_n_vec[is_vae]
    )
  )

  req_keys
}

# ------------------
# REQUEST KEY NAMING
# ------------------

make_sets_name <- function(cat, sca, thr, cha)
{
  sprintf("Sets/%s/S%s_%s_%s.rds", cat, get_sca_ind(sca), thr, cha)
}

make_phate_name <- function(cat, row, col, sca, nor, com, per)
{
  sprintf("PHATE/%s/%s_%s_S%s_N%s_%s_%s.rds",
          cat, row, col, get_sca_ind(sca), get_nor_ind(nor), com, per)
}

make_pvu_name <- function(cat, row, col, sca, nor, emb, vis, com, dim, per, bat)
{
  sca_ind <- get_sca_ind(sca)
  nor_ind <- get_nor_ind(nor)

  if (emb == "PCA")
  {
    if (vis == "Explore")
      return(sprintf("PCA_E/%s/%s_%s_S%s_N%s_%s.rds",
                     cat, row, col, sca_ind, nor_ind, com))
    if (vis == "Summarize")
      return(sprintf("PCA_S/%s/%s_%s_S%s_N%s_%s.rds",
                     cat, row, col, sca_ind, nor_ind, com))
    if (vis == "tSNE")
      return(sprintf("PCA_T/%s/%s_%s_S%s_N%s_%s_%s_%s.rds",
                     cat, row, col, sca_ind, nor_ind, com, dim, per))
  }

  if (emb == "VAE")
  {
    if (vis == "Explore")
      return(sprintf("VAE_E/%s/%s_%s_S%s_N%s_%s_%s.rds",
                     cat, row, col, sca_ind, nor_ind, com, bat))
    if (vis == "Summarize")
      return(sprintf("VAE_S/%s/%s_%s_S%s_N%s_%s_%s.rds",
                     cat, row, col, sca_ind, nor_ind, com, bat))
    if (vis == "tSNE")
      return(sprintf("VAE_T/%s/%s_%s_S%s_N%s_%s_%s_%s_%s.rds",
                     cat, row, col, sca_ind, nor_ind, com, dim, per, bat))
  }

  # UMAP
  if (vis == "Explore")
    return(sprintf("UMAP_E/%s/%s_%s_S%s_N%s_%s_%s.rds",
                   cat, row, col, sca_ind, nor_ind, com, per))
  if (vis == "Summarize")
    return(sprintf("UMAP_S/%s/%s_%s_S%s_N%s_%s_%s.rds",
                   cat, row, col, sca_ind, nor_ind, com, per))
  if (vis == "tSNE")
    return(sprintf("UMAP_T/%s/%s_%s_S%s_N%s_%s_%s_%s.rds",
                   cat, row, col, sca_ind, nor_ind, com, dim, per))
}

make_sdr_name <- function(cat, row, col, sca, nor, emb, vis,
                          com, dim, per, bat, thr, cha)
{
  if (emb == "Sets")
    return(make_sets_name(cat, sca, thr, cha))

  if (emb == "PHATE")
    return(make_phate_name(cat, row, col, sca, nor, com, per))

  # PCA, VAE, UMAP
  make_pvu_name(cat, row, col, sca, nor, emb, vis, com, dim, per, bat)
}

# converts request keys into the locations of their final files
make_key_names <- function(req_keys)
{
  stopifnot(are_req_keys(req_keys))
  n <- nrow(req_keys)
  result <- character(n)

  for (i in seq_len(n))
  {
    args <- as.list(req_keys[i,])
    names(args) <- as.character(req_key_members)
    result[i] <- do.call(make_sdr_name, args)
  }

  result
}

make_req_keys <- function(
    cat = character(), row = character(), col = character(), sca = character(),
    nor = character(), emb = character(), vis = character(), com = numeric(),
    dim = numeric(), per = numeric(), bat = numeric(), thr = numeric(),
    cha = character()
)
{
  clean_req_keys(data.frame(
    "CATEGORIES" = cat,
    "ROW_SUBSETS" = row, "COL_SUBSETS" = col,
    "SCALING" = sca, "NORMALIZATION" = nor,
    "EMBEDDING" = emb, "VISUALIZATION" = vis,
    "COMPONENT" = com, "DIMENSION" = dim,
    "PERPLEXITY" = per, "BATCH_SIZE" = bat,
    "THRESHOLD" = thr,
    "CHARACTERISTIC" = cha
  ))
}

# if the inputs do not correspond to a valid set of requests, return NULL.
# otherwise, return a set of requests in the appropriate form.

# note: the first 13 attributes MUST be a primary key;
#   author / timestamps don't differentiate
# note: if the TIME_COMPLETED field is before the TIME_REQUESTED field,
#   it hasn't been done yet!
# note: a set of requests is valid ONLY IF it has a FILE_LOCATION field.
#   This is because regular computation of the FILE_LOCATION field
#   makes merging new requests difficult. Duplicate values are not allowed.
make_requests <- function(
    cat = character(), row = character(), col = character(), sca = character(),
    nor = character(), emb = character(), vis = character(), com = numeric(),
    dim = numeric(), per = numeric(), bat = numeric(), thr = numeric(),
    cha = character(), aut = character()
){
  # it's not a valid request if the lengths of all attributes aren't equal
  requests <- make_req_keys(
    cat, row, col, sca, nor,
    emb, vis, com, dim, per, bat, thr, cha)

  n_req <- nrow(requests)
  stopifnot(is_str(aut, n_req))

  key_names <- make_key_names(requests) # still req_keys
  stopifnot(is_unique(key_names))

  requests$AUTHOR <- aut
  day_in_seconds <- 60 * 60 * 24
  time_vec <- rep(Sys.time(), n_req)
  requests$TIME_REQUESTED <- time_vec
  requests$TIME_COMPLETED <- time_vec - day_in_seconds
  requests$FILE_LOCATION <- key_names

  requests
}

# ---------------
# SYNTACTIC SUGAR
# ---------------

# pca, vae, umap
make_pvu_requests <- function(
    cat = character(), row = character(), col = character(), sca = character(),
    nor = character(), emb = character(), vis = character(), com = numeric(),
    dim = numeric(), per = numeric(), bat = numeric(), aut = character()
)
{
  n_cat <- length(cat)

  make_requests(
    cat, row, col, sca, nor, emb, vis,
    com, dim, per, bat, rep(num_d, n_cat), rep(chr_d, n_cat), aut)
}

# simplifies the generation of PHATE requests
make_phate_requests <- function(
    cat = character(), row = character(), col = character(), sca = character(),
    nor = character(), com = numeric(), per = numeric(), aut = character()
)
{
  n_cat <- length(cat)

  make_requests(
    cat, row, col, sca, nor, rep("PHATE", n_cat), rep(chr_d, n_cat), com,
    rep(num_d, n_cat), per, rep(num_d, n_cat),
    rep(num_d, n_cat), rep(chr_d, n_cat), aut)
}

# simplifies the generation of Sets requests
make_sets_requests <- function(
    cat = character(), sca = character(), thr = numeric(),
    cha = character(), aut = character())
{
  n_cat <- length(cat)

  make_requests(
    cat, rep(chr_d, n_cat), rep(chr_d, n_cat), sca,
    rep("Global Min-Max", n_cat), rep("Sets", n_cat), rep(chr_d, n_cat),
    rep(num_d, n_cat), rep(num_d, n_cat), rep(num_d, n_cat),
    rep(num_d, n_cat),thr, cha, aut)
}

# ---------------
# REQUEST MERGING
# ---------------

# merges two sets of requests, assuming they are both valid individually.
# If two requests have the same final location, we must choose which one to keep.
# If one request is complete and the other isn't, keep the complete one.
# If it's a tie, pick the one completed more recently.
# If it's still a tie, pick the one in requests1.
# Then rbind, preserving the same order as rbind(requests1, requests2).
# note: chain together with r1 %>% rbind_req2(r2) %>% rbind_req2(r3) %>% ...
rbind_req2 <- function(requests1, requests2)
{
  n1 <- nrow(requests1)
  n2 <- nrow(requests2)
  r1_locs <- requests1$FILE_LOCATION
  r2_locs <- requests2$FILE_LOCATION
  r1_com_times <- requests1$TIME_COMPLETED
  r1_req_times <- requests1$TIME_REQUESTED

  req_dict <- hash::hash()

  # key based on final location, value is (row, TIME_COMPLETED, done)
  for (r1_i in which(r1_locs %in% r2_locs))
  {
    r1_com_time <- r1_com_times[r1_i]
    req_dict[[r1_locs[r1_i]]] <- c(r1_i, r1_com_time,
                                   (r1_com_time > r1_req_times[r1_i]))
  }

  # now go through all intersecting indices
  r2_com_times <- requests2$TIME_COMPLETED
  r2_req_times <- requests2$TIME_REQUESTED
  r1_keep <- rep(TRUE, n1)
  r2_keep <- rep(TRUE, n2)

  for (r2_i in which(r2_locs %in% r1_locs))
  {
    r1_val <- req_dict[[r2_locs[r2_i]]]
    r1_i <- r1_val[1]
    r1_com_time <- r1_val[2]
    r1_done <- r1_val[3]
    r2_com_time <- r2_com_times[r2_i]
    r2_done <- (r2_com_time > r2_req_times[r2_i])

    if (r1_done)
    {
      if (r2_done)
      {
        if (r1_com_time < r2_com_time)
        {
          r1_keep[r1_i] <- FALSE
        }
        else
        {
          r2_keep[r2_i] <- FALSE
        }
      }
      else
      {
        r2_keep[r2_i] <- FALSE
      }
    }
    else
    {
      if (r2_done)
      {
        r1_keep[r1_i] <- FALSE
      }
      else
      {
        if (r1_com_time < r2_com_time)
        {
          r1_keep[r1_i] <- FALSE
        }
        else
        {
          r2_keep[r2_i] <- FALSE
        }
      }
    }
  }

  rbind(requests1[r1_keep,], requests2[r2_keep,])
}

# vectorization of rbind_req
rbind_req <- function(...)
{
  list_of_requests <- list(...)
  n <- length(list_of_requests)
  if (n < 1)
    return(NULL)

  result <- NULL

  for (requests in list_of_requests)
  {
    if (is.null(result))
      result <- requests
    else
      result <- rbind_req2(result, requests)
  }

  rownames(result) <- NULL
  result
}

# ----------------
# PRESENT REQUESTS
# ----------------

# prettifies a request table for use by the library DT
present_requests <- function(requests)
{
  recent_reqs_first <- order(requests$TIME_COMPLETED, decreasing = TRUE)
  res <- requests[recent_reqs_first, , drop = FALSE]
  rownames(res) <- NULL

  # only specify time completed down to the day; if it's not completed, show NA
  res$TIME_REQUESTED <- as.Date(res$TIME_REQUESTED, format = "%Y-%m-%d")
  res$TIME_COMPLETED <- as.Date(res$TIME_COMPLETED, format = "%Y-%m-%d")
  for (i in seq_len(nrow(res)))
    if (res$TIME_COMPLETED[i] < res$TIME_REQUESTED[i])
      res$TIME_COMPLETED[i] <- NA
  res
}

cat_f("REQUEST MANAGER TIME: %.1f (sec)\n", net_time())
