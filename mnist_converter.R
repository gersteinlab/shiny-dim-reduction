# The goal of this script is to convert test numerical data
# into numeric data and metadata.
project_name <- "mnist"
setwd(sprintf("%s/shiny-dim-reduction", Sys.getenv("SHINY_DIM_REDUCTION_ROOT")))
source("converter.R", encoding="UTF-8")

categories_full <- list(
  "MNIST"=list(
    "Training_Data"=784,
    "Test_Data"=784
  )
)

init_cat()

# from https://tensorflow.rstudio.com/guide/tfestimators/examples/mnist/
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

mnist <- rapply(sources, classes = "character", how = "list", function(url) {
  # download + extract the file at the URL
  target <- file.path(raw_loc, basename(url))
  if (!file.exists(target))
    download.file(url, target)

  # read the IDX file
  read_idx(target)
})

setwd(pro_loc)

indices <- sample(1:10000, 1000, replace=FALSE)

combined_train <- convert_to_num(mnist$train$x[indices,])
combined_test <- convert_to_num(mnist$test$x[indices,])
saveRDS(combined_train, "combined/combined_Training_Data.rds")
saveRDS(combined_test, "combined/combined_Test_Data.rds")

order_total <- my_empty_list(name_cat)

order_total[[1]] <- data.frame(matrix(mnist$train$y, ncol=1))[indices,,drop=FALSE]
colnames(order_total[[1]]) <- "Number"
order_total[[2]] <- data.frame(matrix(mnist$test$y, ncol=1))[indices,,drop=FALSE]
colnames(order_total[[2]]) <- "Number"

amazon_keys <- list(
  "id" = "AKIAVI2HZGPOKCJWL3VR",
  "secret" = "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",
  "bucket" = "shiny-app-data-justin-mnist"
)
pc_cap <- 10
perplexity_types <- c(5, 10, 20, 50, 100)
app_title <- "Dimensionality Reduction Tool for MNIST"

setwd(dep_loc)
self_save(c("categories_full", "amazon_keys", "order_total", "pc_cap",
            "perplexity_types", "app_title"))
