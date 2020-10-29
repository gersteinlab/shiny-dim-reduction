# The goal of this script is to convert UMAP to vis data.

library(dbscan)

# --------------
# USER VARIABLES
# --------------

source("~/Justin-Tool/shiny-dim-reduction/scaling.R")

# ---------
# FUNCTIONS
# ---------

# gets the number of noisy samples after an hdbscan
num_noisy <- function(data)
{
  length(data[data == 0])
}

# fast way to get the number of noisy samples after sampling the first cols 
# of a data frame
my_hdbscan <- function(data, cols)
{
  num_noisy(hdbscan(data[,1:cols,drop=FALSE], minPts = 5)$cluster)
}

# ---------------------
# PERFORM VISUALIZATION
# ---------------------

empty_results <- matrix(0, nrow=(pc_cap+1)*(length(perplexity_types)+1), ncol=3)

dog <- name_cat
for (cat in dog)
{
  for (sub in sub_groups[[cat]])
  {
    for (sca in sca_options)
    {
      for (nor in nor_options)
      {
        for (fea in c(1, 10, 100))
        {
          loc <- sprintf("%s_%s_%s_%s_%s.rds", fea, nor, sca, sub, cat)
          print(loc)
          
          none <- perplexity_list
          tsne2 <- perplexity_list
          tsne3 <- perplexity_list

          for (nei in perplexity_types)
          {
            explore <- readRDS(sprintf("UMAP/UMAP-%s_%s", nei, loc))$layout

            # NONE
            none[[sprintf("P%s", nei)]] <- explore

            # tSNE
            tsne2[[sprintf("P%s", nei)]] <- my_rTSNE(explore, 2, nei)$Y
            tsne3[[sprintf("P%s", nei)]] <- my_rTSNE(explore, 3, nei)$Y
          }

          saveRDS(none, sprintf("vis-UMAP/NONE_UMAP_%s", loc))
          saveRDS(list("TSNE2"=tsne2, "TSNE3"=tsne3),
                  sprintf("vis-UMAP/TSNE_UMAP_%s", loc))
          
          # SUM
          raw <- readRDS(sprintf("combined/combined_%s.rds", cat))
          raw <- get_safe_sub(sub, raw, decorations, cat)
          raw <- do_scal(sca, raw)
          raw <- do_norm(nor, raw)
          raw <- feature_start(raw, fea/100)
          
          results <- empty_results
          for (iter1 in 0:length(perplexity_types))
          {
            # iter2 = 0
            my_row <- iter1*(pc_cap+1)+1
            results[my_row,1] <- 0
            results[my_row,2] <- nrow(raw)
            results[my_row,3] <- ifelse(iter1 == 0, "Standard",
                                        sprintf("UMAP-%s", perplexity_types[iter1]))
            
            for (iter2 in 1:pc_cap)
            {
              my_row <- iter1*(pc_cap+1)+iter2+1
              results[my_row,1] <- iter2
              results[my_row,3] <- ifelse(iter1 == 0, "Standard",
                                          sprintf("UMAP-%s", perplexity_types[iter1]))
            }
          }
          
          for (hdim in 1:pc_cap)
            results[hdim+1,2] <- my_hdbscan(raw, hdim)
          
          for (nei in 1:length(perplexity_types))
          {
            umap <- readRDS(sprintf("UMAP/UMAP-%s_%s", 
                                    perplexity_types[nei], loc))$layout
            
            for (hdim in 1:pc_cap)
              results[nei*(pc_cap+1)+hdim+1,2] <- my_hdbscan(umap, hdim)
          }
          saveRDS(results, sprintf("vis-UMAP/SUM_UMAP_%s", loc))
        }
      }
    }
  }
}