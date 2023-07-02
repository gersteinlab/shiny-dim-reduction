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

# ---------------
# ANALYSIS NAMING
# ---------------

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

# -----------------------------
# REQUEST VALIDATION / CREATION
# -----------------------------

# default integer value
int_d <- function(n = 1)
{
  rep(-1L, n)
}

# default finite value
fin_d <- function(n = 1)
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

request_members <- c(
  "CATEGORIES" = "cat",
  "SCALING" = "sca",
  "NORMALIZATION" = "nor",
  "EMBEDDING" = "emb",
  "ROW_SUBSETS" = "row",
  "COL_SUBSETS" = "col",
  "VISUALIZATION" = "vis",
  "COMPONENT" = "com",
  "DIMENSION" = "dim",
  "PERPLEXITY" = "per",
  "BATCH_SIZE" = "bat",
  "THRESHOLD" = "thr",
  "CHARACTERISTIC" = "cha"
)

# are these requests?
are_requests <- function(x)
{
  is.data.frame(x) && identical(names(x), names(request_members)) &&
    is.character(x$CATEGORIES) &&
    is.character(x$SCALING) &&
    is.character(x$NORMALIZATION) &&
    is.character(x$EMBEDDING) &&
    is.character(x$ROW_SUBSETS) &&
    is.character(x$COL_SUBSETS) &&
    is.character(x$VISUALIZATION) &&
    is.integer(x$COMPONENT) &&
    is.integer(x$DIMENSION) &&
    is.integer(x$PERPLEXITY) &&
    is.finite(x$THRESHOLD) &&
    is.character(x$CHARACTERISTIC)
}

# cleans a request (a requests data.frame with a single row) and returns the cleaned version,
# returning NULL if the request cannot be properly cleaned.
# note: should only be called through make_requests, assuming valid types
# note: if an attribute is unused, we set it to a default value (ex: batch_size to NaN for PCA)
clean_request <- function(request)
{
  # must be a single request
  if (!are_requests(request) || nrow(request) != 1)
    return(NULL)

  # abbreviations
  cat <- request$CATEGORIES
  sca <- request$SCALING
  nor <- request$NORMALIZATION
  row <- request$ROW_SUBSETS
  col <- request$COL_SUBSETS
  emb <- request$EMBEDDING
  vis <- request$VISUALIZATION
  com <- request$COMPONENT
  dim <- request$DIMENSION
  per <- request$PERPLEXITY
  bat <- request$BATCH_SIZE
  thr <- request$THRESHOLD
  cha <- request$CHARACTERISTIC

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
    request$ROW_SUBSETS <- chr_d()
    request$COL_SUBSETS <- chr_d()
    request$VISUALIZATION <- chr_d()
    request$COMPONENT <- int_d()
    request$DIMENSION <- int_d()
    request$PERPLEXITY <- int_d()
    request$BATCH_SIZE <- int_d()

    return(request)
  }
  else
  {
    # a valid normalization is required
    if (!(nor %in% nor_options))
      return(NULL)

    # clean set-related attributes
    request$THRESHOLD <- fin_d()
    request$CHARACTERISTIC <- chr_d()

    # must be a valid row subset
    row_num <- row_subset_lengths[[cat]][[row]]
    if (!is.integer(row_num))
      return(NULL)

    # must be a valid column subset
    col_num <- col_subset_lengths[[cat]][[col]]
    is (!is.integer(col_num))
      return(NULL)

    # avoid out-of-range perplexities
    max_perplexity <- as.integer((row_num - 1) / 3)

    # PHATE
    if (emb == "PHATE")
    {
      # must be reduced down to 2 or 3 dimensions for plotting
      if (com != 2L && com != 3L)
        return(NULL)

      # avoid out-of-range perplexities
      if (!dplyr::between(per, 1L, max_perplexity))
        return(NULL)

      # clean all irrelevant components
      request$VISUALIZATION <- chr_d()
      request$DIMENSION <- int_d()
      request$BATCH_SIZE <- int_d()

      return(request)
    }
    else # PCA, VAE, UMAP
    {
      if (vis %nin% vis_options)
        return(NULL)

      if (vis == "tSNE")
      {
        # must go to 2D or 3D
        if (dim != 2 && dim != 3)
          return(NULL)

        # components must be between
        if (!dplyr::between(com, dim, col_num - 1))
          return(NULL)


        # avoid out-of-range perplexity
        if (!dplyr::between(per, 0, max_perplexity))
          return(NULL)
      }
      else
      {
        # avoid out-of-range components
        if (!dplyr::between(com, 1, col_num - 1))
          return(NULL)

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

# converts requests into the locations of their final files
# note: a set of requests is invalid if the following occurs:
#   rel_fin_locs <- requests_to_final(requests)
#   sum(duplicated(rel_fin_locs)) > 0
requests_to_final <- function(requests)
{
  n <- nrow(requests)
  result <- character(n)

  for (i in seq_len(n))
  {
    args <- as.list(requests[i,])
    names(args) <- NULL
    result[i] <- do.call(make_sdr_name, args[1:13])
  }

  result
}

# if the inputs do not correspond to a valid set of requests, return NULL.
# otherwise, return a set of requests in the appropriate form.
# com: components for first-round dimensionality reduction
# dim: dimension for second-round dimensionality reduction
# note: the first 13 attributes MUST be a primary key; author / timestamps don't differentiate
# note: if the TIME_COMPLETED field is before the TIME_REQUESTED field, it hasn't been done yet!
# note: a set of requests is valid ONLY IF it has a FILE_LOCATION field. This is because regular
# computation of the FILE_LOCATION field makes merging new requests difficult.
# note: init_cat, init_sub, and get_dependency("order_total") must be run before this.
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
    "TIME_REQUESTED" = times_requested,
    "TIME_COMPLETED" = times_requested - 60 * 60 * 24 # minus 1 day
  )

  # cleans all requests, quitting if it's not possible to do so
  for (i in seq_len(nrow(requests)))
  {
    clean_req <- clean_request(requests[i,])
    if (is.null(clean_req))
      return(NULL)
    requests[i,] <- clean_req
  }

  requests$FILE_LOCATION <- requests_to_final(requests)

  # quit if two requests have duplicate final locations
  if (sum(duplicated(requests$FILE_LOCATION)) > 0)
    return(NULL)

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
get_request_id <- function(n = 1)
{
  # if AWS is not available, IDs cannot be generated
  if (!key_is_connected())
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

colnames_ids_first <- c("REQUEST_ID", colnames(make_requests()))

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
