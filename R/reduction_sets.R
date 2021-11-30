# The goal of this script is to perform UpSet analysis on the data.
# Note: Given the large sizes of the matrices, the possible values for
# the threshold should be precomputed. Even so, it's a challenge!

if (!exists("ran_install"))
{
  if (file.exists("install.R"))
    source("install.R")
  else
    stop("Could not confirm installation. Please source install.R manually.")
}

source_sdr("preprocess.R")

library(Matrix)

# ---------
# FUNCTIONS
# ---------

# target[i, j] returns whether data[i, j] >= cutoff,
# removing columns with no values at the cutoff or above
calculate_sets <- function(data, cutoff) {
  target <- matrix(as.numeric(data >= cutoff), nrow=nrow(data), dimnames = dimnames(data))
  target[, colSums(target) > 0, drop = FALSE]
}

# given a binary matrix SETS from calculate_sets, let final[feature][label] be the
# fraction of samples with that label where that feature was present in SETS
set_label_matrix <- function(sets, labels){
  # validate that this is a binary matrix
  stopifnot(all.equal(class(matrix()), class(sets)))
  stopifnot(sum(sets == 0) + sum(sets == 1) == nrow(sets) * ncol(sets))
  # validate that labels is a vector of characters
  stopifnot(is.character(labels))

  rownames_final <- colnames(sets)
  summary <- summary(Matrix(sets, sparse = TRUE))
  summary_i <- summary[, 1]
  summary_j <- summary[, 2]

  set_types <- unique(labels)
  num_types <- length(set_types)
  rowlen_final <- length(rownames_final)

  final <- rep(0, rowlen_final*num_types)
  lookup <- match(labels, set_types)
  lookup2 <- (lookup - 1) * rowlen_final

  for (len in 1:length(summary_i))
  {
    num_i <- summary_i[len]
    index <- lookup2[num_i] + summary_j[len]
    final[index] <- final[index] + 1
  }

  final <- matrix(final, ncol=num_types)

  numbers <- rep(0, num_types)
  for (a in lookup)
    numbers[a] <- numbers[a] + 1
  for (j in 1:num_types)
    final[,j] <- final[,j] / numbers[j]

  dimnames(final) <- list(rownames_final, set_types)
  final
}

# searches for a threshold to numdigits precision
# such that calculate_sets(data, thre) approximates target
binary_search <- function(data, target, numdigits)
{
  precision <- 0.1^numdigits
  lower <- precision
  upper <- 1
  while (upper-lower >= precision)
  {
    mid <- (lower + upper)/2

    if (sum(colSums(data >= mid) > 0) < target)
      upper <- mid
    else
      lower <- mid
  }

  round((lower+upper)/2,numdigits)
}

# selects only worthwhile characteristics
select_chars <- function(order){
  select_if(order, function(x){
    between(length(unique(x)), 2, num_filters)
  })
}

# --------------
# USER VARIABLES
# --------------

# threshold caps
lower <- 8 # 2^3
upper <- 262144 # 2^18
numdigits <- 4
len_inter <- 10

# Only allow filtering on a characteristic with <= num_filters distinct values.
num_filters <- 60

# ------------
# SET ANALYSIS
# ------------

thresholds <- empty_named_list(sca_options)
for (sca in sca_options)
  thresholds[[sca]] <- empty_named_list(name_cat)

setwd(pro_loc)
dog <- name_cat
for (cat in dog)
{
  short_list <- select_chars(order_total[[cat]])

  final_saver <- empty_named_list(colnames(short_list))
  print(sprintf("For %s: %s", cat, paste(names(final_saver), collapse=" ")))

  combined <- readRDS(sprintf("combined/combined_%s.rds", cat))

  for (sca in sca_options)
  {
    scaled <- combined %>% do_scal(sca, .) %>% do_norm(nor_options[1], .)

    local_lower <- binary_search(scaled, upper, numdigits-1)
    local_upper <- binary_search(scaled, lower, numdigits-1)

    if (local_lower > 1 - 10^(1-numdigits))
      local_lower <- 1 - 10^(1-numdigits)

    if (local_upper < local_lower + 10^(1-numdigits))
      local_upper <- local_lower + 10^(1-numdigits)

    print(sprintf("(%s, %s) for %s", local_lower, local_upper, sca))

    diff <- (local_upper - local_lower)/len_inter
    chord <- round(seq(local_lower, local_upper, diff), 4)
    thresholds[[sca]][[cat]] <- chord

    for (ind in 1:(len_inter+1))
    {
      target <- calculate_sets(scaled, chord[ind])

      for (cha in colnames(short_list))
        final_saver[[cha]] <- gather_char(target, short_list[[cha]])

      saveRDS(final_saver, sprintf("Sets/Sets-%s_%s_%s.rds", ind, sca, cat))
    }
  }
}

setwd(dep_loc)
saveRDS(thresholds, "thresholds.rds")
