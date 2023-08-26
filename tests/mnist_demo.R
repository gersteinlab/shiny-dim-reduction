# The goal of this file is to build an application using MNIST data.

source("app/storage.R")
source("app/preprocess.R")
source("pipeline/workflows.R")

# upsert_workflow("MNIST", "C:/Users/justin/Desktop/CodeR/DataR/sdr_workflows")
load_wf_config()
cat_wf_config()
set_workflow("MNIST")

# cloud_store, cloud_store_admin must be stored separately
save_local_store(get_loc_store())
set_store_mode("local")
load_all_stores()

# -------------
# DOWNLOAD DATA
# -------------

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

raw_loc <- get_loc_wf("raw")
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

mkdir_saveRDS(combined_train, get_cat_table_name("Train"))
mkdir_saveRDS(combined_test, get_cat_table_name("Test"))

app_data$title <- "Dimensionality Reduction Tool for MNIST"
app_data$citations <- "
Train Images: <a href=\"https://storage.googleapis.com/cvdf-datasets/mnist/train-images-idx3-ubyte.gz\" target=\"_blank\">
https://storage.googleapis.com/cvdf-datasets/mnist/train-images-idx3-ubyte.gz</a>
<br>
Train Labels: <a href=\"https://storage.googleapis.com/cvdf-datasets/mnist/train-labels-idx1-ubyte.gz\" target=\"_blank\">
https://storage.googleapis.com/cvdf-datasets/mnist/train-labels-idx1-ubyte.gz</a>
<br>
Test Images: <a href=\"https://storage.googleapis.com/cvdf-datasets/mnist/t10k-images-idx3-ubyte.gz\" target=\"_blank\">
https://storage.googleapis.com/cvdf-datasets/mnist/t10k-images-idx3-ubyte.gz</a>
<br>
Test Labels: <a href=\"https://storage.googleapis.com/cvdf-datasets/mnist/t10k-labels-idx1-ubyte.gz\" target=\"_blank\">
https://storage.googleapis.com/cvdf-datasets/mnist/t10k-labels-idx1-ubyte.gz</a><br><br>"

app_data$row_axes <- list(
  "Train" = make_axis(
    row_meta_train,
    subsets = list(),
    rel_meta = "Number"
  ),
  "Test" = make_axis(
    row_meta_test,
    subsets = list(),
    rel_meta = "Number"
  )
)

for (i in as.character(0:9))
{
  app_data$row_axes$Train$subsets[[i]] <- which(
    app_data$row_axes$Train$metadata$Number == i)
  app_data$row_axes$Test$subsets[[i]] <- which(
    app_data$row_axes$Test$metadata$Number == i)
}

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
copy_app_to_wf_msg()
