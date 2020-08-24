# The goal of this script is to perform PCA on the data.

source("~/Justin-Tool/code/scaling.R")

# ----------------------------
# PRINCIPAL COMPONENT ANALYSIS
# ----------------------------

dog <- names(categories)
for (cat in dog)
{
  combined <- myRDS(sprintf("combined/combined_%s.rds", cat))
  
  for (sub in sub_groups[[cat]])
  {
    scaled <- get_safe_sub(sub, combined, decorations, cat)
    
    for (sca in sca_options)
    {
      scaled <- do_scal(sca, scaled)
      
      for (nor in nor_options)
      {
        scaled <- do_norm(nor, scaled)
        
        for (fea in c(1, 10, 100))
        {
          pca_title <- sprintf("PCA/PCA_%s_%s_%s_%s_%s.rds", fea, nor, sca, sub, cat)
          scaled <- feature_start(scaled, 1.0*fea/100)
          print(pca_title)
          pca <- prcomp(scaled, center = TRUE, rank. = pc_cap) 
          
          # print(dim(pca$rotation))
          # 
          # rotate <- pca$rotation[,1:3]
          # ind <- rep(0, nrow(rotate))
          # for (i in 1:nrow(rotate))
          # {
          #   target <- abs(rotate[i, ])
          #   sum_ind <- which(target > sum(target)*0.5)
          #   
          #   if (length(sum_ind) > 0)
          #     ind[i] <- sum_ind
          # }
          # pca$rotation <- NULL
          # pca$center <- NULL
      
          myRDS(pca_title, pca)
        }
      }
    }
  }
}

# pca <- prcomp(scaled, center = TRUE, rank. = pc_cap) 
# lol <- prcomp(pca$rotation, center = TRUE, rank. = 3)
# target <- lol$x
# 
# c1 <- apply(pca$rotation[,1:3], 1, function(x){
#   target <- abs(x)
#   sum_ind <- which(target > sum(target)*0.6)
#   names(sum_ind) <- NULL
#   
#   if (length(sum_ind) > 0)
#     return(sum_ind)
#   return(0)
# })
# indices <- which(c1 != 0)
# 
# c2 <- as.factor(c1[indices])
# bruh <- lol[indices,]