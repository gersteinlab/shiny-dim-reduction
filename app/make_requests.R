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

#' gets the index of sca in sca_options
#'
#' @param sca [string]
#' @returns [integer]
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

#' gets the index of nor in nor_options
#'
#' @param nor [string]
#' @returns [integer]
get_nor_ind <- function(nor)
{
  which(nor_options == nor)
}

#' gets the nor_options available to an embedding
#'
#' @param emb [string]
#' @returns [character]
nor_options_by_emb <- function(emb)
{
  # for Sets, Global Min-Max normalization is required
  if (emb == "Sets")
    return("Global Min-Max")
  # for VAE, require normalizations that stay in [0, 1]
  if (emb == "VAE")
    return(c("Global Min-Max", "Local Min-Max"))
  nor_options
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

# req_keys determine the analysis performed
# and are transformed into the names of
# all final analysis file locations
req_key_members <- c(
  "CATEGORIES",
  "ROW_SUBSETS",
  "COL_SUBSETS",
  "SCALING",
  "NORMALIZATION",
  "EMBEDDING",
  "VISUALIZATION",
  "COMPONENT",
  "DIMENSION",
  "PERPLEXITY",
  "BATCH_SIZE",
  "THRESHOLD",
  "CHARACTERISTIC"
)

#' is the data.frame x a 'req_keys' object?
#'
#' @param x [data.frame] not checked
#' @returns [boolean]
are_req_keys <- function(x)
{
  is.character(x$CATEGORIES) &&
    is.character(x$ROW_SUBSETS) &&
    is.character(x$COL_SUBSETS) &&
    is.character(x$SCALING) &&
    is.character(x$NORMALIZATION) &&
    is.character(x$EMBEDDING) &&
    is.character(x$VISUALIZATION) &&
    is.integer(x$COMPONENT) &&
    is.integer(x$DIMENSION) &&
    is.integer(x$PERPLEXITY) &&
    is.integer(x$BATCH_SIZE) &&
    is.numeric(x$THRESHOLD) &&
    is.character(x$CHARACTERISTIC)
}

int_d <- -1L  # default integer value
num_d <- -1   # default numeric value
chr_d <- "-"  # default character value

rep_sum <- function(value, x)
{
  rep(value, sum(x))
}

#' cleans request keys, stopping on any inconsistency
#' note: sets unused attributes to default values
#'
#' @param req_keys [req_keys]
#' @returns [req_keys]
clean_req_keys <- function(req_keys)
{
  stopifnot(
    is.data.frame(req_keys),
    are_req_keys(req_keys)
  )

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

  req_cat_names <- unique(cat_vec)

  stopifnot(
    req_cat_names %in% cat_names,
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
  is_sp <- is_sets | is_phate

  # set all unnecessary attributes to default values
  req_keys$ROW_SUBSETS[is_sets] <- chr_d %>% rep_sum(is_sets)
  req_keys$COL_SUBSETS[is_sets] <- chr_d %>% rep_sum(is_sets)
  req_keys$VISUALIZATION[is_sp] <- chr_d %>% rep_sum(is_sp)
  req_keys$COMPONENT[is_sets] <- int_d %>% rep_sum(is_sets)
  req_keys$BATCH_SIZE[!is_vae] <- int_d %>% rep_sum(!is_vae)
  req_keys$THRESHOLD[!is_sets] <- num_d %>% rep_sum(!is_sets)
  req_keys$CHARACTERISTIC[!is_sets] <- chr_d %>% rep_sum(!is_sets)

  # do the same for the values that depend on earlier cleaning
  is_tsne <- (vis_vec == "tSNE")
  uses_per <- (is_umap | is_phate | is_tsne)
  uses_dim <- (is_sets | is_phate | !is_tsne)
  req_keys$DIMENSION[uses_dim] <- int_d %>% rep_sum(uses_dim)
  req_keys$PERPLEXITY[!uses_per] <- int_d %>% rep_sum(!uses_per)

  # ----------
  # VALIDATION
  # ----------

  stopifnot(
    nor_vec[is_vae] %in% nor_options_by_emb("VAE"),
    nor_vec[is_sets] %in% nor_options_by_emb("Sets"),
    # for Sets, thresholds between 0 and 1 are required
    vec_between(thr_vec[is_sets], 0, 1),
    # for PVU, visualization options must be valid
    vis_vec[is_pvu] %in% vis_options,
    # for PHATE, the end result must be 2 or 3 dimensions
    com_vec[is_phate] %in% c(2, 3),
    # for tSNE, the end result must be 2 or 3 dimensions
    dim_vec[is_tsne] %in% c(2, 3)
  )

  # for Sets, characteristics must be in relevant metadata cols
  for (cat in req_cat_names)
  {
    rel_meta <- get_row_axis(cat)$rel_meta
    stopifnot(cha_vec[is_sets & (cat_vec == cat)] %in% rel_meta)
  }

  # get the number of rows / columns for various subsets
  row_n_vec <- rep(NA, nrow(req_keys))
  col_n_vec <- rep(NA, nrow(req_keys))

  for (cat in req_cat_names)
  {
    is_cat <- (cat_vec == cat)

    row_sub_lens <- get_row_sub_lengths(cat)
    for (row in names(row_sub_lens))
      row_n_vec[is_cat & (row_vec == row)] <- row_sub_lens[[row]]

    col_sub_lens <- get_col_sub_lengths(cat)
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

#' creates req_keys from the input
#'
#' @returns [req_keys]
make_req_keys <- function(
    cat = character(), row = character(), col = character(), sca = character(),
    nor = character(), emb = character(), vis = character(),
    com = integer(), dim = integer(), per = integer(), bat = integer(),
    thr = numeric(), cha = character()
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

# ------------------
# REQUEST KEY NAMING
# ------------------

#' name the file where a PCA req_key is stored
#'
#' @returns [string]
name_pca_file <- function(cat, row, col, sca, nor, vis, com, dim, per)
{
  sca_ind <- get_sca_ind(sca)
  nor_ind <- get_nor_ind(nor)

  if (vis == "Explore")
    return(sprintf("PCA_E/%s/%s_%s_S%d_N%d_%d.rds",
                   cat, row, col, sca_ind, nor_ind, com))
  if (vis == "Summarize")
    return(sprintf("PCA_S/%s/%s_%s_S%d_N%d_%d.rds",
                   cat, row, col, sca_ind, nor_ind, com))
  if (vis == "tSNE")
    return(sprintf("PCA_T/%s/%s_%s_S%d_N%d_%d_%d_%d.rds",
                   cat, row, col, sca_ind, nor_ind, com, dim, per))

  stop_f("Visualization mode '%s' is invalid.", vis)
}

#' name the file where a VAE req_key is stored
#'
#' @returns [string]
name_vae_file <- function(cat, row, col, sca, nor, vis, com, dim, per, bat)
{
  sca_ind <- get_sca_ind(sca)
  nor_ind <- get_nor_ind(nor)

  if (vis == "Explore")
    return(sprintf("VAE_E/%s/%s_%s_S%d_N%d_%d_%d.rds",
                   cat, row, col, sca_ind, nor_ind, com, bat))
  if (vis == "Summarize")
    return(sprintf("VAE_S/%s/%s_%s_S%d_N%d_%d_%d.rds",
                   cat, row, col, sca_ind, nor_ind, com, bat))
  if (vis == "tSNE")
    return(sprintf("VAE_T/%s/%s_%s_S%d_N%d_%d_%d_%d_%d.rds",
                   cat, row, col, sca_ind, nor_ind, com, dim, per, bat))

  stop_f("Visualization mode '%s' is invalid.", vis)
}

#' name the file where a UMAP req_key is stored
#'
#' @returns [string]
name_umap_file <- function(cat, row, col, sca, nor, vis, com, dim, per)
{
  sca_ind <- get_sca_ind(sca)
  nor_ind <- get_nor_ind(nor)

  if (vis == "Explore")
    return(sprintf("UMAP_E/%s/%s_%s_S%d_N%d_%d_%d.rds",
                   cat, row, col, sca_ind, nor_ind, com, per))
  if (vis == "Summarize")
    return(sprintf("UMAP_S/%s/%s_%s_S%d_N%d_%d_%d.rds",
                   cat, row, col, sca_ind, nor_ind, com, per))
  if (vis == "tSNE")
    return(sprintf("UMAP_T/%s/%s_%s_S%d_N%d_%d_%d_%d.rds",
                   cat, row, col, sca_ind, nor_ind, com, dim, per))

  stop_f("Visualization mode '%s' is invalid.", vis)
}

#' name the file where a PCA / VAE / UMAP req_key is stored
#'
#' @returns [string]
name_pvu_file <- function(cat, row, col, sca, nor, emb, vis, com, dim, per, bat)
{
  if (emb == "PCA")
    return(name_pca_file(cat, row, col, sca, nor, vis, com, dim, per))
  if (emb == "VAE")
    return(name_vae_file(cat, row, col, sca, nor, vis, com, dim, per, bat))
  if (emb == "UMAP")
    return(name_umap_file(cat, row, col, sca, nor, vis, com, dim, per))

  stop_f("Embedding %s is not one of PCA, VAE, UMAP.", emb)
}

#' name the file where a Sets req_key is stored
#'
#' @returns [string]
name_phate_file <- function(cat, row, col, sca, nor, com, per)
{
  sprintf("PHATE/%s/%s_%s_S%d_N%d_%d_%d.rds",
          cat, row, col, get_sca_ind(sca), get_nor_ind(nor), com, per)
}

#' name the file where a Sets req_key is stored
#'
#' @returns [string]
name_sets_file <- function(cat, sca, thr, cha)
{
  sprintf("Sets/%s/S%s_%.4f_%s.rds", cat, get_sca_ind(sca), thr, cha)
}

#' name the file where a req_key is stored
#'
#' @returns [string]
name_req_key_file <- function(cat, row, col, sca, nor, emb, vis,
                              com, dim, per, bat, thr, cha)
{
  if (emb == "PHATE")
    return(name_phate_file(cat, row, col, sca, nor, com, per))
  if (emb == "Sets")
    return(name_sets_file(cat, sca, thr, cha))

  name_pvu_file(cat, row, col, sca, nor, emb, vis, com, dim, per, bat)
}

#' names the files where req_keys are stored
#'
#' @returns [character]
name_req_key_files <- function(req_keys)
{
  stopifnot(are_req_keys(req_keys))
  n <- nrow(req_keys)
  result <- character(n)

  for (i in seq_len(n))
  {
    args <- as.list(req_keys[i,])
    names(args) <- c("cat", "row", "col", "sca", "nor", "emb", "vis",
                     "com", "dim", "per", "bat", "thr", "cha")
    result[i] <- do.call(name_req_key_file, args)
  }

  result
}

# ------------------
# REQUEST MANAGEMENT
# ------------------

#' Is x a 'requests' object?
#'
#' ANATOMY OF A REQUEST:
#' --first 13 columns: request key (req_key)
#' --last 4 columns:
#' ----author
#' ----time requested
#' ----time completed
#' ----file location
#'
#' @param x [object]
#' @returns [boolean]
are_requests <- function(x)
{
  members <- c(
    req_key_members,
    "AUTHOR",
    "TIME_REQUESTED",
    "TIME_COMPLETED",
    "FILE_LOCATION"
  )

  is.data.frame(x) && identical(names(x), members) &&
    is.character(x$AUTHOR) &&
    is.POSIXct(x$TIME_REQUESTED) &&
    is.POSIXct(x$TIME_COMPLETED) &&
    is.character(x$FILE_LOCATION)
}

# note: the first 13 attributes MUST be a primary key;
#   author / timestamps don't differentiate
# note: if the TIME_COMPLETED field is before the TIME_REQUESTED field,
#   it hasn't been done yet!
# note: a set of requests is valid ONLY IF it has a FILE_LOCATION field.
#   This is because regular computation of the FILE_LOCATION field
#   makes merging new requests difficult. Duplicate locations are not allowed.
make_requests <- function(
    cat = character(), row = character(), col = character(), sca = character(),
    nor = character(), emb = character(), vis = character(), com = integer(),
    dim = integer(), per = integer(), bat = integer(), thr = numeric(),
    cha = character(), aut = character()
){
  # it's not a valid request if the lengths of all attributes aren't equal
  requests <- make_req_keys(
    cat, row, col, sca, nor,
    emb, vis, com, dim, per, bat, thr, cha)

  key_names <- name_req_key_files(requests) # still req_keys
  stopifnot(!anyDuplicated(key_names))

  requests$AUTHOR <- aut
  cur_time <- rep(Sys.time(), nrow(requests)) # needed in case 0 rows
  requests$TIME_REQUESTED <- cur_time
  day_in_seconds <- 60 * 60 * 24
  requests$TIME_COMPLETED <- cur_time - day_in_seconds
  requests$FILE_LOCATION <- key_names

  requests
}

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

# ---------------
# SYNTACTIC SUGAR
# ---------------

# pca, vae, umap
make_pvu_requests <- function(
    cat = character(), row = character(), col = character(), sca = character(),
    nor = character(), emb = character(), vis = character(), com = integer(),
    dim = integer(), per = integer(), bat = integer(), aut = character()
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
    nor = character(), com = integer(), per = integer(), aut = character()
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
