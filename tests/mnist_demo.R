# The goal of this script is to convert test numerical data
# into numeric data and metadata.

sources <- list(
  train = list(
    x = "https://storage.googleapis.com/cvdf-datasets/mnist/train-images-idx3-ubyte.gz",
    y = "https://storage.googleapis.com/cvdf-datasets/mnist/train-labels-idx1-ubyte.gz"
  ),
  test = list(
    x = "https://storage.googleapis.com/cvdf-datasets/mnist/t10k-images-idx3-ubyte.gz",
    y = "https://storage.googleapis.com/cvdf-datasets/mnist/t10k-labels-idx1-ubyte.gz"
  )
)

# read an MNIST file (encoded in IDX format)
read_idx <- function(file) {
  # create binary connection to file
  conn <- gzfile(file, open = "rb")
  on.exit(close(conn), add = TRUE)

  # read the magic number as sequence of 4 bytes
  magic <- readBin(conn, what = "raw", n = 4, endian = "big")
  ndims <- as.integer(magic[[4]])

  # read the dimensions (32-bit integers)
  dims <- readBin(conn, what = "integer", n = ndims, endian = "big")

  # read the rest in as a raw vector
  data <- readBin(conn, what = "raw", n = prod(dims), endian = "big")

  # convert to an integer vecto
  converted <- as.integer(data)

  # return plain vector for 1-dim array
  if (length(dims) == 1)
    return(converted)

  # wrap 3D data into matrix
  matrix(converted, nrow = dims[1], ncol = prod(dims[-1]), byrow = TRUE)
}

source("app/storage.R")
source("pipeline/workflows.R")
load_wf_config()
list_workflows()
ensure_stores("local")

# upsert_workflow("MNIST", "C:/Users/justin/Desktop/CodeR/DataR/sdr_workflows")

set_current_workflow("exRNA")
cloud_store_admin <- readRDS(cloud_store_admin_loc())
cloud_store_admin$bucket <- "shiny-app-data-justin-mnist"
copy_app_to_wf_msg()

set_current_workflow("MNIST")
list_workflows()
mkdir_saveRDS(cloud_store_admin, cloud_store_admin_loc())

local_store <- get_loc_rel_wf(prepend_store())

cloud_store <- list(
  "id" = "AKIAVI2HZGPOMRT4LPHZ",
  "secret" = "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",
  "bucket" = "shiny-app-data-justin-mnist"
)

save_local_store()
save_cloud_store()

raw_loc <- get_loc_rel_wf("raw")
ensure_dir(raw_loc)

mnist <- rapply(sources, classes = "character", how = "list", function(url) {
  # download + extract the file at the URL
  target <- file.path(raw_loc, basename(url))
  if (!file.exists(target))
    download.file(url, target)

  # read the IDX file
  read_idx(target)
})

combined_train <- mnist$train$x
combined_test <- mnist$test$x
row_meta_train <- data.frame(
  "Index" = as.character(seq_along(mnist$train$y)),
  "Number" = as.character(mnist$train$y))
row_meta_test <- data.frame(
  "Index" = as.character(seq_along(mnist$test$y)),
  "Number" = as.character(mnist$test$y)
)
col_meta_train <- data.frame(
  "Pixel" = as.character(seq_len(ncol(combined_train)))
)
col_meta_test <- data.frame(
  "Pixel" = as.character(seq_len(ncol(combined_test)))
)

stopifnot(
  is_table(combined_train),
  is_table(combined_test),
  is_metadata(row_meta_train),
  is_metadata(row_meta_test),
  is_metadata(col_meta_train),
  is_metadata(col_meta_test)
)

#' gets the table name for a category
#'
#' @param cat [string] not checked
#' @returns [string]
get_cat_table_name <- function(cat)
{
  sprintf("combined_%s.rds", cat) %>% prepend_table() %>% get_loc_rel_wf()
}

mkdir_saveRDS(combined_train, get_cat_table_name("Train"))
mkdir_saveRDS(combined_test, get_cat_table_name("Test"))

source("app/preprocess.R")

app_data$title <- "Dimensionality Reduction Tool for MNIST"
app_data$citations <- ""

app_data$row_axes <- list(
  "Train" = make_axis(
    row_meta_train,
    subsets = list(
      "Mini" = 1:600
    ),
    rel_meta = "Number"
  ),
  "Test" = make_axis(
    row_meta_test,
    subsets = list(
      "Mini" = 1:600
    ),
    rel_meta = "Number"
  )
)

app_data$col_axes <- list(
  "Train" = make_axis(col_meta_train, rel_meta = character()),
  "Test" = make_axis(col_meta_test, rel_meta = character())
)

app_data$categories <- list(
  "Train" = make_category("Train", "Train"),
  "Test" = make_category("Test", "Test")
)

app_data$groups <- list(
  "MNIST" = c("Train", "Test")
)

stopifnot(is_app_data(app_data))
save_app_data()

