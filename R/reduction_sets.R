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
source_sdr("red_methods.R")

# ---------
# FUNCTIONS
# ---------

# searches for a threshold to numdigits precision
# such that table_to_sets(data, thre) approximates target
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

# --------------
# USER VARIABLES
# --------------

# threshold caps
lower <- 8 # 2^3
upper <- 262144 # 2^18
numdigits <- 4
len_inter <- 10

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
      target <- table_to_sets(scaled, chord[ind])

      for (cha in colnames(short_list))
        final_saver[[cha]] <- set_label_matrix(target, short_list[[cha]])

      saveRDS(final_saver, sprintf("Sets/Sets-%s_%s_%s.rds", ind, sca, cat))
    }
  }
}

# setwd(dep_loc)
# saveRDS(thresholds, "thresholds.rds")
