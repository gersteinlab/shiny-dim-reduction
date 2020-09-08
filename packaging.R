# The goal of this script is to convert vis data to packaged data.

# --------------
# USER VARIABLES
# --------------

source("~/Justin-Tool/shiny-dim-reduction/scaling.R")

# -------------------
# PACKAGE PCA and VAE
# -------------------

# normal
for (emb in c("PCA", "VAE"))
{
  for (sca in sca_options)
  {
    for (nor in nor_options)
    {
      for (fea in c(1, 10, 100))
      {
        # explore and sum
        setwd(pro_loc)
        explore <- my_empty_list(name_cat)
        sum <- explore 
        for (cat in names(categories))
        {
          explore[[cat]] <- my_empty_list(sub_groups[[cat]])
          for (sub in sub_groups[[cat]])
          {
            loc <- sprintf("%s_%s_%s_%s_%s.rds", fea, nor, sca, sub, cat)
            explore[[cat]][[sub]] <- myRDS(
              sprintf("vis-%s/NONE_%s_%s", emb, emb, loc))
            sum[[cat]][[sub]] <- myRDS(
              sprintf("vis-%s/SUM_%s_%s", emb, emb, loc))
          }
        }
        setwd(dat_loc)
        myRDS(make_file_name(
          sca, nor, fea, emb, "Explore", "", ""),
          explore)
        myRDS(make_file_name(
          sca, nor, fea, emb, "Summarize", "", ""),
          sum)
        
        # tsne
        for (nei in perplexity_types)
        {
          for (dim in c(2,3))
          {
            setwd(pro_loc)
            tsne <- my_empty_list(name_cat)
            for (cat in names(categories))
            {
              tsne[[cat]] <- my_empty_list(sub_groups[[cat]])
              for (sub in sub_groups[[cat]])
              {
                loc <- sprintf("%s_%s_%s_%s_%s.rds", fea, nor, sca, sub, cat)
                tsne_vis <- myRDS(sprintf("vis-%s/TSNE_%s_%s", emb, emb, loc))
                tsne[[cat]][[sub]] <- tsne_vis[[
                  sprintf("TSNE%s", dim)]][[sprintf("P%s", nei)]]
              }
            }
            setwd(dat_loc)
            myRDS(make_file_name(
              sca, nor, fea, emb, "tSNE", dim, 
              which(perplexity_types == nei)),
              tsne)
          }
        }
      }
    }
  }
}

# AWS
setwd(pro_loc)
dog <- names(categories)
for (cat in dog)
{
  print(cat)
  print(Sys.time())
  
  for (sub in sub_groups[[cat]])
  {
    for (sca in sca_options)
    {
      for (nor in nor_options)
      {
        for (fea in c(1, 10, 100))
        {
          for (emb in c("PCA", "VAE"))
          {
            loc <- sprintf("%s_%s_%s_%s_%s.rds", fea, nor, sca, sub, cat)
            # loc <- repStr(loc, sprintf("Total_%s", name_cat), name_cat)
            
            save_db(
              myRDS(sprintf("vis-%s/NONE_%s_%s", emb, emb, loc)),
              aws_bucket,
              make_aws_name(
                make_file_name(sca, nor, fea, emb, "Explore", "", ""), 
                sub, cat
              )
            )
            
            save_db(
              myRDS(sprintf("vis-%s/SUM_%s_%s", emb, emb, loc)), 
              aws_bucket,
              make_aws_name(
                make_file_name(sca, nor, fea, emb, "Summarize", "", ""), 
                sub, cat
              )
            )
            
            tsne_vis <- myRDS(sprintf("vis-%s/TSNE_%s_%s", emb, emb, loc))
            
            for (nei in perplexity_types)
            {
              nei_ind <- which(perplexity_types == nei)
              
              for (dim in c(2,3))
              {
                save_db(
                  tsne_vis[[sprintf("TSNE%s", dim)]][[sprintf("P%s", nei)]],
                  aws_bucket,
                  make_aws_name(
                    make_file_name(sca, nor, fea, emb, "tSNE", dim, nei_ind), 
                    sub, cat
                  )
                )
              }
            }
          }
        }
      }
    }
  }
}

# ------------
# PACKAGE UMAP
# ------------

# normal
for (sca in sca_options)
{
  for (nor in nor_options)
  {
    for (fea in c(1, 10, 100))
    {
      # sum
      setwd(pro_loc)
      sum <- my_empty_list(name_cat)
      for (cat in names(categories))
      {
        sum[[cat]] <- my_empty_list(sub_groups[[cat]])
        for (sub in sub_groups[[cat]])
        {
          loc <- sprintf("%s_%s_%s_%s_%s.rds", fea, nor, sca, sub, cat)
          sum[[cat]][[sub]] <- myRDS(sprintf("vis-UMAP/SUM_UMAP_%s", loc))
        }
      }
      setwd(dat_loc)
      myRDS(make_file_name(
        sca, nor, fea, "UMAP", "Summarize", "", ""),
        sum)
      
      # explore and tsne
      for (nei in perplexity_types)
      {
        setwd(pro_loc)
        explore <- my_empty_list(name_cat)
        for (cat in names(categories))
        {
          explore[[cat]] <- my_empty_list(sub_groups[[cat]])
          for (sub in sub_groups[[cat]])
          {
            loc <- sprintf("%s_%s_%s_%s_%s.rds", fea, nor, sca, sub, cat)
            umap_vis <- myRDS(sprintf("vis-UMAP/NONE_UMAP_%s", loc))
            explore[[cat]][[sub]] <- umap_vis[[sprintf("P%s", nei)]]
          }
        }
        setwd(dat_loc)
        myRDS(make_file_name(
          sca, nor, fea, "UMAP", "Explore", "",
          which(perplexity_types == nei)),
          explore)
        
        for (dim in c(2,3))
        {
          setwd(pro_loc)
          tsne <- my_empty_list(name_cat)
          for (cat in names(categories))
          {
            tsne[[cat]] <- my_empty_list(sub_groups[[cat]])
            for (sub in sub_groups[[cat]])
            {
              loc <- sprintf("%s_%s_%s_%s_%s.rds", fea, nor, sca, sub, cat)
              tsne_vis <- myRDS(sprintf("vis-UMAP/TSNE_UMAP_%s", loc))
              tsne[[cat]][[sub]] <- tsne_vis[[
                sprintf("TSNE%s", dim)]][[sprintf("P%s", nei)]]
            }
          }
          setwd(dat_loc)
          myRDS(make_file_name(
            sca, nor, fea, "UMAP", "tSNE", dim,
            which(perplexity_types == nei)),
            tsne)
        }
      }
    }
  }
}

# AWS
setwd(pro_loc)
dog <- rev(names(categories))
for (cat in dog)
{
  print(cat)
  print(Sys.time())
  
  for (sub in sub_groups[[cat]])
  {
    for (sca in sca_options)
    {
      for (nor in nor_options)
      {
        for (fea in c(1, 10, 100))
        {
          loc <- sprintf("%s_%s_%s_%s_%s.rds", fea, nor, sca, sub, cat)
          # loc <- repStr(loc, sprintf("Total_%s", name_cat), name_cat)
          
          save_db(
            myRDS(sprintf("vis-UMAP/SUM_UMAP_%s", loc)), 
            aws_bucket,
            make_aws_name(
              make_file_name(sca, nor, fea, "UMAP", "Summarize", "", ""), 
              sub, cat
            )
          )
          
          explore <- myRDS(sprintf("vis-UMAP/NONE_UMAP_%s", loc))
          
          for (nei in perplexity_types)
          {
            nei_ind <- which(perplexity_types == nei)
            
            save_db(
              explore[[sprintf("P%s", nei)]], 
              aws_bucket,
              make_aws_name(
                make_file_name(sca, nor, fea, "UMAP", "Explore", "", nei_ind), 
                sub, cat
              )
            )
            
            tsne_vis <- myRDS(sprintf("vis-UMAP/TSNE_UMAP_%s", loc))
            
            for (dim in c(2,3))
            {
              save_db(
                tsne_vis[[sprintf("TSNE%s", dim)]][[sprintf("P%s", nei)]],
                aws_bucket,
                make_aws_name(
                  make_file_name(sca, nor, fea, "UMAP", "tSNE", dim, nei_ind), 
                  sub, cat
                )
              )
            }
          }
        }
      }
    }
  }
}

# -------------
# PACKAGE PHATE
# -------------

# normal
for (sca in sca_options)
{
  for (nor in nor_options)
  {
    for (fea in c(1, 10, 100))
    {
      for (nei in perplexity_types)
      {
        for (dim in c(2,3))
        {
          setwd(pro_loc)
          phate <- my_empty_list(name_cat)
          for (cat in names(categories))
          {
            phate[[cat]] <- my_empty_list(sub_groups[[cat]])
            for (sub in sub_groups[[cat]])
            {
              phate[[cat]][[sub]] <- myRDS(sprintf(
                "PHATE/PHATE-%s-%s_%s_%s_%s_%s_%s.rds", 
                nei, dim, fea, nor, sca, sub, cat))$embedding
            }
          }
          setwd(dat_loc)
          myRDS(make_file_name(sca, nor, fea, 
                               "PHATE", "", dim, 
                               which(perplexity_types == nei)), phate)
        }
      }
    }
  }
}

# AWS
setwd(pro_loc)
dog <- rev(names(categories))
for (cat in dog)
{
  print(cat)
  print(Sys.time())
  
  for (sub in sub_groups[[cat]])
  {
    for (sca in sca_options)
    {
      for (nor in nor_options)
      {
        for (fea in c(1, 10, 100))
        {
          for (nei in perplexity_types)
          {
            nei_ind <- which(perplexity_types == nei)
            
            for (dim in c(2,3))
            {
              phate <- myRDS(sprintf(
                "PHATE/PHATE-%s-%s_%s_%s_%s_%s_%s.rds",
                nei, dim, fea, nor, sca, sub, cat))$embedding
              
              # phate <- myRDS(sprintf(
              #   "PHATE/PHATE-%s-%s_%s_%s_%s_%s.rds", 
              #   nei, dim, fea, nor, sca, cat))$embedding
              
              save_db(
                phate,
                aws_bucket,
                make_aws_name(
                  make_file_name(sca, nor, fea, "PHATE", "", dim, nei_ind), 
                  sub, cat
                )
              )
            }
          }
        }
      }
    }
  }
}

# Sets
num_filters <- 60
dog <- names(categories)
for (cat in dog)
{
  print(cat)
  print(Sys.time())
  
  order <- order_total[[cat]]
  
  short_list <- select_if(order, function(x){
    between(length(unique(x)), 2, num_filters)
  })

  for (sca in sca_options) 
  {
    sca_ind <- which(sca_options == sca)
    
    for (ind in 11:1)
    {
      set_data <- myRDS(sprintf("Sets/Sets-%s_%s_%s.rds", 
                                ind, sca, cat))
      
      for (cha in colnames(short_list))
      {
        save_db(
          set_data[[cha]],
          aws_bucket,
          sprintf("Sets/Sets-%s_%s_%s_%s.rds", 
                  ind, sca_ind, cha, cat)
        )
      }
    }
  }
}