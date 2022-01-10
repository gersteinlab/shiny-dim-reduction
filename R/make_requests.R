# The purpose of this file is to store functions for validating,
# creating, and combining reduction requests.

source_sdr("preprocess.R")

# -----------------------------
# REQUEST VALIDATION / CREATION
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

# selects only metadata features with a limited number of values
select_chars <- function(order, num_filters = 60){
  select_if(order, function(x){
    between(length(unique(x)), 2, num_filters)
  })
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

# ---------------
# REQUEST MERGING
# ---------------

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
