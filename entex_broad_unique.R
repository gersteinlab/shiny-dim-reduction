setwd(sprintf("%s/shiny-dim-reduction", Sys.getenv("SHINY_DIM_REDUCTION_ROOT")))
source("pipeline.R", encoding="UTF-8")
source("app_functions.R", encoding="UTF-8")

# a threshold parameter - can be from 1 to 11, 1 is least strict
thresh <- 2

# -----------
# COMPUTATION
# -----------

setwd(dep_loc)

# scale options 
sca_options <- c("Logarithmic", "Linear")

# load files
decorations <- readRDS("decorations.rds")
order_total <- readRDS("order_total.rds")
categories_full <- readRDS("categories_full.rds")

# create categories
init_cat()

# calculate
setwd(pro_loc)
sets <- my_empty_list(names(categories))
summary <- sets

for (cat in names(categories))
{
  sets[[cat]] <- my_empty_list(sca_options)
  summary[[cat]] <- sets[[cat]]
  
  for (sca in sca_options)
  {
    sets[[cat]][[sca]] <- readRDS(
      sprintf("Sets/Sets-%s_%s_%s.rds", thresh, sca, cat))$TISSUE %>% 
      set_f1_f2(c(0.4, 1), c(0, 60)) %>% num_nan_binary()
    summary[[cat]][[sca]] <- apply(sets[[cat]][[sca]], 1, sum)
  }
}

table <- matrix(ncol=3, nrow=length(categories)*length(sca_options))
colnames(table) <- c("Total Tissue Number", 
                     "Uniquely (single tissue)", 
                     "Broadly (>90% all tissues)")
rownames(table) <- 1:(length(sca_options)*num_cat)

for (cn in 1:length(categories))
{
  cat <- names(categories)[cn]
  for (sn in 1:2)
  {
    sca <- sca_options[sn]
    print(sprintf("%s %s:", cat, sca), quote=FALSE)
    ind <- 2*cn + sn - 2
    print(ind)
    
    numbers <- summary[[cat]][[sca]]
    
    rownames(table)[ind] <- sprintf("%s (%s)", cat, sca)
    table[ind, 1] <- length(numbers)
    table[ind, 2] <- sum(between(numbers, 1, 1))
    table[ind, 3] <- sum(between(numbers, 0.9*max(numbers), max(numbers))
    )
  }
}

setwd(sprintf("%s/yucheng_example", roo_loc))
write.csv(table, file="table.csv")
