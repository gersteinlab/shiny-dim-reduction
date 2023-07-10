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
  "COMPONENT" = "com",
  # dim: dimension for second-round dimensionality reduction
  "DIMENSION" = "dim",
  # see red_methods.R
  "PERPLEXITY" = "per",
  # see red_methods.R
  "BATCH_SIZE" = "bat",
  # see red_methods.R
  "THRESHOLD" = "thr",
  # see red_methods.R
  "CHARACTERISTIC" = "cha"
)

# default integer value
int_d <- function(n = 1)
{
  rep(-1L, n)
}

# default numeric value
num_d <- function(n = 1)
{
  rep(-1, n)
}

# default author value
aut_d <- function(n = 1)
{
  rep("ADMIN", n)
}

# default character value
chr_d <- function(n = 1)
{
  rep("-", n)
}

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
    is.integer(x$COMPONENT) &&
    is.integer(x$DIMENSION) &&
    is.integer(x$PERPLEXITY) &&
    is.numeric(x$THRESHOLD) &&
    is.character(x$CHARACTERISTIC)
}

# cleans a request key (a req_keys data.frame with a single row) and
# returns the cleaned version or NULL if the request cannot be properly cleaned.
# note: should only be called through make_req_keys, assuming valid types
# note: if an attribute is unused, we set it to the default value.
clean_req_key <- function(req_key)
{
  # abbreviations
  cat <- req_key$CATEGORIES
  row <- req_key$ROW_SUBSETS
  col <- req_key$COL_SUBSETS
  sca <- req_key$SCALING
  nor <- req_key$NORMALIZATION
  emb <- req_key$EMBEDDING
  vis <- req_key$VISUALIZATION
  com <- req_key$COMPONENT
  dim <- req_key$DIMENSION
  per <- req_key$PERPLEXITY
  bat <- req_key$BATCH_SIZE
  thr <- req_key$THRESHOLD
  cha <- req_key$CHARACTERISTIC

  if (!(cat %in% cat_names))
    return(NULL)

  if (!(emb %in% emb_options))
    return(NULL)

  if (!(sca %in% sca_options))
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
    cha_options <- get_safe_chas(cat)
    if (!(cha %in% cha_options))
      return(NULL)

    # clean all irrelevant attributes
    req_key$ROW_SUBSETS <- chr_d()
    req_key$COL_SUBSETS <- chr_d()
    req_key$VISUALIZATION <- chr_d()
    req_key$COMPONENT <- int_d()
    req_key$DIMENSION <- int_d()
    req_key$PERPLEXITY <- int_d()
    req_key$BATCH_SIZE <- int_d()

    return(req_key)
  }
  else
  {
    # a valid normalization is required
    if (!(nor %in% nor_options))
      return(NULL)

    # clean set-related attributes
    req_key$THRESHOLD <- num_d()
    req_key$CHARACTERISTIC <- chr_d()

    # must be a valid row subset
    row_n <- row_subset_lengths[[cat]][[row]]
    if (!is.integer(row_n))
      return(NULL)

    # must be a valid column subset
    col_n <- col_subset_lengths[[cat]][[col]]
    is (!is.integer(col_n))
    return(NULL)

    # PHATE
    if (emb == "PHATE")
    {
      # must be reduced down to 2 or 3 dimensions for plotting
      if (com != 2L && com != 3L)
        return(NULL)

      # avoid out-of-range perplexity
      if (!perplexity_is_valid(per, row_n))
        return(NULL)

      # clean all irrelevant components
      req_key$VISUALIZATION <- chr_d()
      req_key$DIMENSION <- int_d()
      req_key$BATCH_SIZE <- int_d()

      return(req_key)
    }
    else # PCA, VAE, UMAP
    {
      if (!(vis %in% vis_options))
        return(NULL)

      if (vis == "tSNE")
      {
        # must go to 2D or 3D
        if (dim != 2L && dim != 3L)
          return(NULL)

        # components must be between
        if (!dplyr::between(com, dim, col_n))
          return(NULL)

        # avoid out-of-range perplexity
        if (!perplexity_is_valid(per, row_n))
          return(NULL)
      }
      else
      {
        # avoid out-of-range / unplottable components
        if (!dplyr::between(com, 2L, col_n))
          return(NULL)

        # no second-level reduction? no DIMENSION parameter needed
        req_key$DIMENSION <- int_d()
      }

      if (emb == "VAE")
      {
        # prevent normalizations that don't end in the range [0, 1]
        if (nor != "Global Min-Max" && nor != "Local Min-Max")
          return(NULL)

        # avoid out-of-range batch sizes
        if (!dplyr::between(bat, 1L, row_n))
          return(NULL)

        if (vis != "tSNE")
          req_key$PERPLEXITY <- int_d()

        return(req_key)
      }
      else
      {
        req_key$BATCH_SIZE <- int_d()

        if (emb == "UMAP")
        {
          # avoid non-integral or negative perplexities
          if (!perplexity_is_valid(per, row_n))
            return(NULL)

          return(req_key)
        }
        else # emb == "PCA" at this stage
        {
          if (vis != "tSNE")
            request$PERPLEXITY <- int_d()

          return(req_key)
        }
      }
    }
  }

  # should never be reached?
  return(NULL)
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

make_sdr_name <- function(cat, row, col, sca, nor, emb, vis, com, dim, per, bat, thr, cha)
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
    nor = character(), emb = character(), vis = character(), com = integer(),
    dim = integer(), per = integer(), bat = integer(), thr = numeric(),
    cha = character()
)
{
  req_keys <- data.frame(
    "CATEGORIES" = cat,
    "ROW_SUBSETS" = row, "COL_SUBSETS" = col,
    "SCALING" = sca, "NORMALIZATION" = nor,
    "EMBEDDING" = emb, "VISUALIZATION" = vis,
    "COMPONENT" = com, "DIMENSION" = dim,
    "PERPLEXITY" = per, "BATCH_SIZE" = bat,
    "THRESHOLD" = thr,
    "CHARACTERISTIC" = cha
  )

  stopifnot(are_req_keys(req_keys))
  n_keys <- nrow(req_keys)

  for (i in seq_len(n_keys))
  {
    clean_req <- clean_req_key(req_keys[i,])
    if (is.null(clean_req))
      stop_f("Could not clean key %s!", i)
    req_keys[i,] <- clean_req
  }

  req_keys
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
    nor = character(), emb = character(), vis = character(), com = integer(),
    dim = integer(), per = integer(), bat = integer(), thr = numeric(),
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
    com, dim, per, bat, num_d(n_cat), chr_d(n_cat), aut)
}

# simplifies the generation of PHATE requests
make_phate_requests <- function(
    cat = character(), row = character(), col = character(), sca = character(),
    nor = character(), com = numeric(), per = numeric(), aut = character()
)
{
  n_cat <- length(cat)

  make_requests(
    cat, row, col, sca, nor, rep("PHATE", n_cat), chr_d(n_cat),
    com, num_d(n_cat), per, num_d(n_cat), num_d(n_cat), chr_d(n_cat), aut)
}

# simplifies the generation of Sets requests
make_sets_requests <- function(
    cat = character(), sca = character(), thr = numeric(), cha = character(), aut = character())
{
  n_cat <- length(cat)

  make_requests(
    cat, chr_d(n_cat), chr_d(n_cat), sca, rep("Global Min-Max", n_cat), rep("Sets", n_cat), chr_d(n_cat),
    num_d(n_cat), num_d(n_cat), num_d(n_cat), num_d(n_cat), thr, cha, aut)
}

# ---------------
# REQUEST MERGING
# ---------------

# merges two sets of requests under the assumption that they are both valid individually.
# The biggest issue is if a row in requests1 and a row in requests2 both have the same final
# location - then we must choose which one to keep. First, try to pick a completed analysis.
# If it's a tie, pick the one completed more recently. If it's a tie, pick the one in requests1.
# Then rbind, preserving the same order as rbind(requests1, requests2).
# note: chain together with r1 %>% rbind_requests(r2) %>% rbind_requests(r3) %>% ...
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
    req_dict[[r1_locs[r1_i]]] <- c(r1_i, r1_com_time, (r1_com_time > r1_req_times[r1_i]))
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

# ----------
# REQUEST ID
# ----------

# gets n request ids
# warning: only works with access to AWS
get_request_id <- function(n = 1)
{
  # if AWS is not available, IDs cannot be generated
  if (connected != "cloud")
    return(-1)

  if (n < 1)
    return(numeric())

  id <- 1:n
  if (find_aws_s3("Sessions/cur_id.rds"))
    id <- id + load_aws_s3("Sessions/cur_id.rds")

  save_aws_s3(id[n], "Sessions/cur_id.rds")
  id
}

# ----------------
# PRESENT REQUESTS
# ----------------

colnames_ids_first <- c("REQUEST_ID", names(req_key_members))

# prettifies a request table for use by the library DT
present_requests <- function(requests)
{
  recent_reqs_first <- order(requests$TIME_COMPLETED, decreasing = TRUE)
  res <- requests[recent_reqs_first,, drop = FALSE]
  if ("REQUEST_ID" %in% colnames(res))
    res <- res[,colnames_ids_first, drop = FALSE]
  rownames(res) <- NULL

  # only specify time completed down to the day; if it's not completed, show NA
  res$TIME_REQUESTED <- as.Date(res$TIME_REQUESTED, format = "%Y-%m-%d")
  res$TIME_COMPLETED <- as.Date(res$TIME_COMPLETED, format = "%Y-%m-%d")
  for (i in seq_len(nrow(res)))
    if (res$TIME_COMPLETED[i] < res$TIME_REQUESTED[i])
      res$TIME_COMPLETED[i] <- NA
  res
}
