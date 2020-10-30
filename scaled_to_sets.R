# The goal of this script is to perform UpSet analysis on the data.
# Note: Given the large sizes of the matrices, the possible values for
# the threshold should be precomputed. Even so, it's a challenge!

library(Matrix)

# --------------
# USER VARIABLES
# --------------

source("~/Justin-Tool/shiny-dim-reduction/scaling.R")

# threshold caps
lower <- 8 # 2^3
upper <- 262144 # 2^18
numdigits <- 4
len <- 10

# Only allow filtering on a characteristic with <= num_filters distinct values.
num_filters <- 60

# ---------
# FUNCTIONS
# ---------

# Given a lower bound, converts into a binary matrix
calculate_sets <- function(data, low_t) {
  target <- matrix(as.numeric(data >= low_t), nrow=nrow(data), dimnames = dimnames(data))
  target[, colSums(target) > 0, drop = FALSE]
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

# ----------
# THRESHOLDS
# ----------

thresholds <- list(
  "Linear"=my_empty_list(names(categories)), 
  "Logarithmic"=my_empty_list(names(categories))
)

# ------------
# SET ANALYSIS
# ------------

select_chars <- function(order){
  select_if(order, function(x){
    between(length(unique(x)), 2, num_filters)
  })
}

dog <- name_cat
for (cat in dog)
{
  short_list <- select_chars(order_total[[cat]])
  
  final_saver <- my_empty_list(colnames(short_list))
  print(sprintf("For %s: %s", cat, names(final_saver)))
  
  combined <- readRDS(sprintf("combined/combined_%s.rds", cat))
  scaled <- combined
  
  for (sca in sca_options) 
  {
    scaled <- do_scal(sca, scaled)
    
    for (nor in nor_options)
    {
      scaled <- do_norm(nor, scaled)
      
      local_lower <- binary_search(scaled, upper, numdigits-1)
      local_upper <- binary_search(scaled, lower, numdigits-1)
      
      if (local_upper - local_lower < 10^(1-numdigits))
        local_upper <- local_upper + 10^(1-numdigits)
      
      print(sprintf("(%s, %s) for %s, %s", local_lower, local_upper, sca, nor))
      
      thresholds[[sca]][[cat]] <- c(local_lower, local_upper)
      diff <- (local_lower - local_upper)/len
      chord <- round(seq(local_upper, local_lower, diff), 4)
      
      for (ind in len:1)
      {
        thre <- chord[length(chord)-ind+1]
        target <- calculate_sets(scaled, thre) %>% Matrix(sparse = TRUE)
        summary <- summary(target)
        rownames_final <- target@Dimnames[[2]]
        rm(target)
        summary_i <- summary[, 1]
        summary_j <- summary[, 2]
        rm(summary)
        
        for (cha in colnames(short_list))
        {
          associated <- short_list[[cha]]
          
          set_types <- unique(associated)
          num_types <- length(set_types)
          rowlen_final <- length(rownames_final)
          
          final <- rep(0, rowlen_final*num_types)
          lookup <- match(associated, set_types)
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
          
          colnames(final) <- set_types
          rownames(final) <- rownames_final
          final_saver[[cha]] <- final
        }
        
        saveRDS(final_saver, sprintf("Sets/Sets_%s_%s_%s_%s.rds", cat, sca, nor, ind))
      }
    }
  }
}

setwd(dep_loc)
saveRDS(thresholds, "thresholds.rds")