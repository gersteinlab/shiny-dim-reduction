# The purpose of this file is to perform a set of requested reduction analyses.
# Intermediate-level files will be saved in pro_loc/inter
# Final-level files will be saved in ref_loc

if (!exists("ran_install"))
{
  if (file.exists("install.R"))
    source("install.R")
  else
    stop("Could not confirm installation. Please source install.R manually.")
}

source_sdr("sca_nor_fun.R")
source_sdr("red_methods.R")
source_sdr("make_requests.R")
source_sdr("workflows.R")

# create categories and subsets
init_cat()
init_sub(names)

get_dependency("order_total", empty_named_list(name_cat))

get_dependency("amazon_keys")
set_working_key(amazon_keys)

# -----------------------------
# REQUEST FULFILLMENT / STORAGE
# -----------------------------

# note: only used for PCA, VAE, UMAP (anything else gets the default value)
make_inter_name <- function(cat, row, col, sca, nor, emb, com, per, bat)
{
  sca_ind <- which(sca_options == sca)
  nor_ind <- which(nor_options == nor)

  if (emb == "PCA")
  {
    return(sprintf("PCA/%s/%s_%s_S%s_N%s_%s.rds",
                   cat, row, col, sca_ind, nor_ind, com))
  }

  if (emb == "VAE")
  {
    return(sprintf("VAE/%s/%s_%s_S%s_N%s_%s_%s.rds",
                   cat, row, col, sca_ind, nor_ind, com, bat))
  }

  # UMAP
  if (emb == "UMAP")
  {
    return(sprintf("UMAP/%s/%s_%s_S%s_N%s_%s_%s.rds",
                   cat, row, col, sca_ind, nor_ind, com, per))
  }

  chr_d()
}

# converts a request into the locations of their intermediate files
requests_to_inter <- function(requests)
{
  result <- character()

  for (i in seq_len(nrow(requests)))
  {
    args <- as.list(requests[i,])
    names(args) <- NULL
    result <- c(result, do.call(make_inter_name, args[c(1:6, 8, 10:11)]))
  }

  result
}

# readRDS but return NULL if force_inter
inter_readRDS <- function(force, int_loc)
{
  if (force != 2)
    return(w_def_readRDS(int_loc))
  return(NULL)
}

# n_cur: number of analyses currently begun (including the one that just started)
# n_fin: total number of analyses to be done
# start_time: time the analysis started (from Sys.time())
# filename: the name of the analysis file
red_update_msg <- function(n_cur, n_fin, start_time, filename)
{
  cur_eta <- ceiling((n_fin - n_cur + 1) / n_cur * (Sys.time() - start_time) * 1.5)
  sprintf_clean("Begin F%s/%s, ETA %s: %s", n_cur, n_fin, cur_eta, filename)
}

# performs reduction on a group of valid requests,
# parsing requests in a non-sequential order to maximize speed.
# force = 0: if a final-level file already exists, do nothing
# force = 1: override the final-level file
# force = 2: override all intermediate files and the final-level file
# returns the completed version of requests, with TIME_COMPLETED updated.
# note: you can control how a subset of reductions are performed like so:
# subset <- (requests$EMBEDDING == "PCA")
# requests[subset,] <- perform_reduction(requests[subset,], force)
# example:
#     test <- data.frame(matrix(1:25, nrow = 5))
#     subset <- (test$X1 < 4) & (test$X4 %% 2 == 0)
#     test[subset,] <- data.frame(matrix(1:10, nrow = 2))
perform_reduction <- function(requests, force = 0)
{
  # used to make intermediate files easily
  inter_locs <- sprintf("%s/inter/%s", pro_loc, requests_to_inter(requests))
  # used to make final files easily
  rel_fin_locs <- requests$FILE_LOCATION
  final_locs <- sprintf("%s/%s", ref_loc, rel_fin_locs)
  # used to easily edit completion timestamps
  times_done <- requests$TIME_COMPLETED

  # a true-false vector determining if an analysis should be performed
  i_fin <- !file.exists(final_locs) | rep(force > 0, nrow(requests))

  # a true-false vector determining if TIME_COMPLETED < TIME_REQUESTED
  i_not_done <- times_done < requests$TIME_REQUESTED

  # if an analysis is complete (!i_fin) but requests is not updated (i_not_done),
  # simply update the time the analysis was done to the current time.
  touch_times <- !i_fin & i_not_done
  times_done[touch_times] <- rep(Sys.time(), sum(touch_times))

  # used to show progress
  n_fin <- sum(i_fin)
  n_cur <- 0
  start_time <- Sys.time()

  # select the category
  for (cat in unique(requests$CATEGORIES[i_fin]))
  {
    cat_table <- readRDS(sprintf("%s/combined/combined_%s.rds", pro_loc, cat))
    # if the opened table is not valid, you have a problem
    stopifnot(valid_table(cat_table))

    i_cat <- i_fin & (requests$CATEGORIES == cat)
    # used for sets
    short_order <- select_chars(order_total[[cat]])

    # perform scaling
    for (sca in unique(requests$SCALING[i_cat]))
    {
      sca_table <- do_scal(sca, cat_table)
      i_sca <- i_cat & (requests$SCALING == sca)

      # perform normalization
      for (nor in unique(requests$NORMALIZATION[i_sca]))
      {
        nor_table <- do_norm(nor, sca_table)
        i_nor <- i_sca & (requests$NORMALIZATION == nor)
        i_sets <- i_nor & (requests$EMBEDDING == "Sets")

        # ----
        # SETS
        # ----
        for (thr in unique(requests$THRESHOLD[i_sets]))
        {
          i_thr <- i_sets & (requests$THRESHOLD == thr)
          set_result <- table_to_sets(nor_table, thr)

          for (i in which(i_thr)) # only attributes left: characteristic
          {
            r <- requests[i,]
            f_loc <- final_locs[i]

            n_cur <- n_cur + 1
            red_update_msg(n_cur, n_fin, start_time, rel_fin_locs[i])

            set_label_matrix(
              set_result, short_order[[r$CHARACTERISTIC]]) %>% mkdir_saveRDS(f_loc)
            times_done[i] <- Sys.time()
          }
        }

        for (row in unique(requests$ROW_SUBSETS[i_nor]))
        {
          row_table <- get_row_sub(nor_table, cat, row)
          i_row <- i_nor & (requests$ROW_SUBSETS == row)

          for (col in unique(requests$COL_SUBSETS[i_row]))
          {
            col_table <- get_col_sub(row_table, cat, col)
            i_col <- i_row & (requests$COL_SUBSETS == col)

            for (com in unique(requests$COMPONENT[i_col]))
            {
              i_com <- i_col & (requests$COMPONENT == com)
              i_pca <- i_com & (requests$EMBEDDING == "PCA")
              i_vae <- i_com & (requests$EMBEDDING == "VAE")
              i_umap <- i_com & (requests$EMBEDDING == "UMAP")
              i_phate <- i_com & (requests$EMBEDDING == "PHATE")

              # ---
              # PCA
              # ---
              if (sum(i_pca) > 0) # only attributes left: vis [dim] [per]
              {
                # make the intermediate file
                pca_loc <- inter_locs[match(TRUE, i_pca)]
                pca_result <- inter_readRDS(force, pca_loc)
                if (is.null(pca_result))
                {
                  sprintf_clean("Generating PCA INTER: %s", pca_loc)
                  pca_result <- table_to_pca(col_table, com)
                  mkdir_saveRDS(pca_result, pca_loc)
                }

                # make the final files
                for (i in which(i_pca))
                {
                  r <- requests[i,]
                  f_loc <-  final_locs[i]

                  n_cur <- n_cur + 1
                  red_update_msg(n_cur, n_fin, start_time, rel_fin_locs[i])

                  if (r$VISUALIZATION == "Explore")
                    pca_to_explore(pca_result) %>% mkdir_saveRDS(f_loc)

                  if (r$VISUALIZATION == "Summarize")
                    pca_to_summary(pca_result) %>% mkdir_saveRDS(f_loc)

                  if (r$VISUALIZATION == "tSNE")
                    pca_to_tsne(pca_result, r$DIMENSION, r$PERPLEXITY) %>% mkdir_saveRDS(f_loc)

                  times_done[i] <- Sys.time()
                }
              }

              # ---
              # VAE
              # ---
              for (bat in unique(requests$BATCH_SIZE[i_vae]))
              {
                i_bat <- i_vae & (requests$BATCH_SIZE == bat)

                if (sum(i_bat) > 0) # only attributes left: vis [dim] [per]
                {
                  # make the intermediate file
                  vae_loc <- inter_locs[match(TRUE, i_bat)]
                  vae_result <- inter_readRDS(force, vae_loc)
                  if (is.null(vae_result))
                  {
                    sprintf_clean("Generating VAE INTER: %s", vae_loc)
                    vae_result <- table_to_vae(col_table, com, bat)
                    mkdir_saveRDS(vae_result, vae_loc)
                  }

                  # make the final files
                  for (i in which(i_bat))
                  {
                    r <- requests[i,]
                    f_loc <-  final_locs[i]

                    n_cur <- n_cur + 1
                    red_update_msg(n_cur, n_fin, start_time, rel_fin_locs[i])

                    if (r$VISUALIZATION == "Explore")
                      vae_to_explore(vae_result) %>% mkdir_saveRDS(f_loc)

                    if (r$VISUALIZATION == "Summarize")
                      vae_to_summary(vae_result) %>% mkdir_saveRDS(f_loc)

                    if (r$VISUALIZATION == "tSNE")
                      vae_to_tsne(vae_result, r$DIMENSION, r$PERPLEXITY) %>% mkdir_saveRDS(f_loc)

                    times_done[i] <- Sys.time()
                  }
                }
              }

              # ----
              # UMAP
              # ----
              for (per in unique(requests$PERPLEXITY[i_umap]))
              {
                i_per <- i_umap & (requests$PERPLEXITY == per)

                if (sum(i_per) > 0) # only attributes left: [dim]
                {
                  # make the intermediate file
                  umap_loc <- inter_locs[match(TRUE, i_per)]
                  umap_result <- inter_readRDS(force, umap_loc)
                  if (is.null(umap_result))
                  {
                    sprintf_clean("Generating UMAP INTER: %s", umap_loc)
                    umap_result <- table_to_umap(col_table, com, per)
                    mkdir_saveRDS(umap_result, umap_loc)
                  }

                  # make the final files
                  for (i in which(i_per))
                  {
                    r <- requests[i,]
                    f_loc <-  final_locs[i]

                    n_cur <- n_cur + 1
                    red_update_msg(n_cur, n_fin, start_time, rel_fin_locs[i])

                    if (r$VISUALIZATION == "Explore")
                      umap_to_explore(umap_result) %>% mkdir_saveRDS(f_loc)

                    if (r$VISUALIZATION == "Summarize")
                      umap_to_summary(umap_result) %>% mkdir_saveRDS(f_loc)

                    if (r$VISUALIZATION == "tSNE")
                      umap_to_tsne(umap_result, r$DIMENSION) %>% mkdir_saveRDS(f_loc)

                    times_done[i] <- Sys.time()
                  }
                }
              }

              # -----
              # PHATE
              # -----
              for (i in which(i_phate)) # only attribute left: per
              {
                r <- requests[i,]
                f_loc <-  final_locs[i]

                n_cur <- n_cur + 1
                red_update_msg(n_cur, n_fin, start_time, rel_fin_locs[i])

                table_to_phate(col_table, com, r$PERPLEXITY) %>% mkdir_saveRDS(f_loc)
                times_done[i] <- Sys.time()
              }
            }
          }
        }
      }
    }
  }

  requests$TIME_COMPLETED <- times_done
  requests
}

