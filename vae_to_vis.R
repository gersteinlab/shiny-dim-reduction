# The goal of this script is to convert VAE data to vis data.

# --------------
# USER VARIABLES
# --------------

source("~/Justin-Tool/shiny-dim-reduction/scaling.R")

# ---------
# FUNCTIONS
# ---------

# converts records to a summary
sum_names <- c("Training Iterations", "Loss Value", "Loss Type")
rec_to_sum <- function(records)
{
  rec_num <- nrow(records)
  
  for (i in 1:rec_num)
  {
    if (is.na(records[i, 1]) || is.nan(records[i, 1]))
      print(sprintf("NAN Loss: VAE/VAE_%s", loc))
    
    if (is.na(records[i, 2]) || is.nan(records[i, 2]))
      print(sprintf("NAN Val Loss: VAE/VAE_%s", loc))
  }
  
  vae_sum <- cbind.data.frame(
    rep(1:rec_num, 2),
    c(records[,1], records[,2]),
    c(rep("Testing Loss", rec_num), rep("Validation Loss", rec_num))
  )
  
  colnames(vae_sum) <- sum_names
  vae_sum
}

# ---------------------
# PERFORM VISUALIZATION
# ---------------------

dog <- rev(names(categories))
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
          
          vae <- readRDS(sprintf("VAE/VAE_%s", loc))
          explore <- vae$predict
          
          # NONE
          myRDS(sprintf("vis-VAE/NONE_VAE_%s", loc), explore)
          
          # tSNE
          tsne2 <- perplexity_list
          tsne3 <- perplexity_list
          
          start <- my_timer()
          for (nei in perplexity_types)
          {
            tsne2[[sprintf("P%s", nei)]] <- my_rTSNE(explore, 2, nei)$Y
            tsne3[[sprintf("P%s", nei)]] <- my_rTSNE(explore, 3, nei)$Y
          }
          print(my_timer(start))
         
          myRDS(sprintf("vis-VAE/TSNE_VAE_%s", loc),
                list("TSNE2"=tsne2, "TSNE3"=tsne3))
          
          # SUM
          myRDS(sprintf("vis-VAE/SUM_VAE_%s", loc), 
                rec_to_sum(vae$records))
        }
      }
    }
  }
}