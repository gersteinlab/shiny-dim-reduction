# The purpose of this file is to perform requested analyses.

if (!exists("sdr_config") || sdr_config$mode != "pipeline")
  source("app/install.R")
stopifnot(sdr_config$mode == "pipeline")

source("pipeline/transforms.R")
source("pipeline/red_methods.R")
source("pipeline/workflows.R")
source("app/make_requests.R")
source("app/storage.R")

# remember user's previous work
set_store_mode("local")
load_all_stores()
load_wf_config()
cat_wf_config()

#' gets the table name for a category
#'
#' @param cat [string] not checked
#' @returns [string]
get_cat_table_name <- function(cat)
{
  sprintf("combined_%s.rds", cat) %>% get_loc_table()
}

#' stop if table does not align with cat
#'
#' @param cat [string] not checked
validate_cat_table <- function(cat_table, cat)
{
  stopifnot(
    is_table(cat_table),
    nrow(cat_table) == get_row_sub_lengths(cat)$Total,
    ncol(cat_table) == get_col_sub_lengths(cat)$Total
  )
}

#' validates that all tables are correctly sized
#' system.time({validate_cat_tables()})
validate_cat_tables <- function()
{
  for (cat in cat_names)
    readRDS(get_cat_table_name(cat)) %>% validate_cat_table(cat)
  cat_f("Validated tables for %s categories.\n", length(cat_names))
}

# ----------------------
# INTERMEDIATE FILENAMES
# ----------------------

#' name the file where a PCA intermediate analysis is stored
#'
#' @returns [string]
name_pca_inter <- function(cat, row, col, sca, nor, com)
{
  sprintf("PCA/%s/%s_%s_S%s_N%s_%s.rds",
          cat, row, col, get_sca_ind(sca), get_nor_ind(nor), com)
}

#' name the file where a VAE intermediate analysis is stored
#'
#' @returns [string]
name_vae_inter <- function(cat, row, col, sca, nor, com, bat)
{
  sprintf("VAE/%s/%s_%s_S%s_N%s_%s_%s.rds",
          cat, row, col, get_sca_ind(sca), get_nor_ind(nor), com, bat)
}

#' name the file where a UMAP intermediate analysis is stored
#'
#' @returns [string]
name_umap_inter <- function(cat, row, col, sca, nor, com, per)
{
  sprintf("UMAP/%s/%s_%s_S%s_N%s_%s_%s.rds",
          cat, row, col, get_sca_ind(sca), get_nor_ind(nor), com, per)
}


#' name the file where an intermediate analysis is stored
#'
#' @returns [string]
name_req_key_inter <- function(cat, row, col, sca, nor, emb, com, per, bat)
{
  if (emb == "PCA")
    return(name_pca_inter(cat, row, col, sca, nor, com))
  if (emb == "VAE")
    return(name_vae_inter(cat, row, col, sca, nor, com, bat))
  if (emb == "UMAP")
    return(name_umap_inter(cat, row, col, sca, nor, com, per))

  chr_d # for PHATE, Sets
}

#' names the files where intermediate analyses are stored
#'
#' @param req_keys [req_keys]
#' @returns [character]
name_req_key_inters <- function(req_keys)
{
  stopifnot(are_req_keys(req_keys))
  n <- nrow(req_keys)
  result <- character(n)

  key_args <- c("cat", "row", "col", "sca", "nor",
                "emb", "com", "per", "bat")

  for (i in seq_len(n))
  {
    args <- as.list(req_keys[i,])
    names(args) <- c("cat", "row", "col", "sca", "nor", "emb", "vis",
                     "com", "dim", "per", "bat", "thr", "cha")
    result[i] <- do.call(name_req_key_inter, args[key_args])
  }

  result
}

# -------------------
# REQUEST FULFILLMENT
# -------------------

# n_cur: number of analyses currently begun (including the one that just started)
# n_fin: total number of analyses to be done
# filename: the name of the analysis file
red_update_msg <- function(n_cur, n_fin, filename)
{
  cat_f("request %s/%s: %s\n", n_cur, n_fin, filename)
}

# readRDS but return NULL if force_inter
inter_readRDS <- function(force, int_loc)
{
  if (force != 2)
    return(w_def_readRDS(int_loc))
  return(NULL)
}

#' gets time last modified (aka TIME_COMPLETED) for files
#'
#' @param file [character] not checked
#' @returns [POSIXct]
get_time_completed <- function(file)
{
  file.info(file)$mtime
}

#' performs reduction on a group of valid requests,
#' parsing requests in a non-sequential order to maximize speed
#' note: completion status can be determined by file.exists()
#'    and completion time can be determined by get_time_completed()
#' note: you can run perform_reduction on any row subset of requests,
#'    e.g perform_requests(requests[requests$CATEGORY == "miRNA", ])
#'
#' @param requests [requests]
#' @param force [integer]
#'    force = 0: if final file exists, do nothing
#'    force = 1: if inter file exists, only redo final file
#'    force = 2: redo inter file and final file
perform_reduction <- function(requests, force = 0L)
{
  stopifnot(are_requests(requests), force %in% 0:2)

  # intermediate file locations
  rel_inter_locs <- name_req_key_inters(requests[, 1:13])
  inter_locs <- get_loc_inter(rel_inter_locs)

  # final file locations
  rel_fin_locs <- requests$FILE_LOCATION
  final_locs <- get_loc_store(rel_fin_locs)

  # a true-false vector determining if an analysis should be performed
  i_fin <- (!file.exists(final_locs) | rep(force > 0, nrow(requests)))

  # used to show progress
  n_fin <- sum(i_fin)
  n_cur <- 0

  # select the category
  for (cat in unique(requests$CATEGORIES[i_fin]))
  {
    cat_table <- readRDS(get_cat_table_name(cat))
    validate_cat_table(cat_table, cat)
    i_cat <- i_fin & (requests$CATEGORIES == cat)

    # used for sets
    row_meta <- get_row_axis(cat)$metadata

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
        sets_table <- nor_table # ensure columns are available for labeling
        colnames(sets_table) <- get_col_axis(cat)$metadata[[1]]

        for (thr in unique(requests$THRESHOLD[i_sets]))
        {
          i_thr <- i_sets & (requests$THRESHOLD == thr)
          set_result <- table_to_sets(sets_table, thr)

          for (i in which(i_thr)) # only attributes left: characteristic
          {
            r <- requests[i,]
            f_loc <- final_locs[i]

            n_cur <- n_cur + 1
            red_update_msg(n_cur, n_fin, rel_fin_locs[i])

            set_label_matrix(
              set_result, row_meta[[r$CHARACTERISTIC]]) %>% mkdir_saveRDS(f_loc)
          }
        }

        for (row in unique(requests$ROW_SUBSETS[i_nor]))
        {
          if (row == "Total")
            row_table <- nor_table
          else
            row_table <- subset_by_row(nor_table, get_row_sub(cat, row))

          i_row <- i_nor & (requests$ROW_SUBSETS == row)

          for (col in unique(requests$COL_SUBSETS[i_row]))
          {
            if (col == "Total")
              col_table <- row_table
            else
              col_table <- subset_by_col(row_table, get_col_sub(cat, col))

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
              i_pca_first <- match(TRUE, i_pca)
              if (!is.na(i_pca_first)) # only attributes left: vis [dim] [per]
              {
                # make the intermediate file
                pca_loc <- inter_locs[i_pca_first]
                pca_result <- inter_readRDS(force, pca_loc)
                if (is.null(pca_result))
                {
                  cat_f("Generating PCA INTER: %s\n", rel_inter_locs[i_pca_first])
                  pca_result <- table_to_pca(col_table, com)
                  mkdir_saveRDS(pca_result, pca_loc)
                }

                # make the final files
                for (i in which(i_pca))
                {
                  r <- requests[i,]
                  f_loc <-  final_locs[i]

                  n_cur <- n_cur + 1
                  red_update_msg(n_cur, n_fin, rel_fin_locs[i])

                  if (r$VISUALIZATION == "Explore")
                    pca_to_explore(pca_result) %>% mkdir_saveRDS(f_loc)

                  if (r$VISUALIZATION == "Summarize")
                    pca_to_summary(pca_result) %>% mkdir_saveRDS(f_loc)

                  if (r$VISUALIZATION == "tSNE")
                    pca_to_tsne(pca_result, r$DIMENSION, r$PERPLEXITY) %>%
                    mkdir_saveRDS(f_loc)
                }
              }

              # ---
              # VAE
              # ---
              for (bat in unique(requests$BATCH_SIZE[i_vae]))
              {
                i_bat <- i_vae & (requests$BATCH_SIZE == bat)
                i_bat_first <- match(TRUE, i_bat)

                if (!is.na(i_bat_first)) # only attributes left: vis [dim] [per]
                {
                  # make the intermediate file
                  vae_loc <- inter_locs[i_bat_first]
                  vae_result <- inter_readRDS(force, vae_loc)
                  if (is.null(vae_result))
                  {
                    cat_f("Generating VAE INTER: %s\n", rel_inter_locs[i_bat_first])
                    vae_result <- table_to_vae(col_table, com, bat)
                    mkdir_saveRDS(vae_result, vae_loc)
                  }

                  # make the final files
                  for (i in which(i_bat))
                  {
                    r <- requests[i,]
                    f_loc <-  final_locs[i]

                    n_cur <- n_cur + 1
                    red_update_msg(n_cur, n_fin, rel_fin_locs[i])

                    if (r$VISUALIZATION == "Explore")
                      vae_to_explore(vae_result) %>% mkdir_saveRDS(f_loc)

                    if (r$VISUALIZATION == "Summarize")
                      vae_to_summary(vae_result) %>% mkdir_saveRDS(f_loc)

                    if (r$VISUALIZATION == "tSNE")
                      vae_to_tsne(vae_result, r$DIMENSION, r$PERPLEXITY) %>%
                      mkdir_saveRDS(f_loc)
                  }
                }
              }

              # ----
              # UMAP
              # ----
              for (per in unique(requests$PERPLEXITY[i_umap]))
              {
                i_per <- i_umap & (requests$PERPLEXITY == per)
                i_per_first <- match(TRUE, i_per)

                if (!is.na(i_per_first)) # only attributes left: [dim]
                {
                  # make the intermediate file
                  umap_loc <- inter_locs[i_per_first]
                  umap_result <- inter_readRDS(force, umap_loc)
                  if (is.null(umap_result))
                  {
                    cat_f("Generating UMAP INTER: %s\n", rel_inter_locs[i_per_first])
                    umap_result <- table_to_umap(col_table, com, per)
                    mkdir_saveRDS(umap_result, umap_loc)
                  }

                  # make the final files
                  for (i in which(i_per))
                  {
                    r <- requests[i,]
                    f_loc <-  final_locs[i]

                    n_cur <- n_cur + 1
                    red_update_msg(n_cur, n_fin, rel_fin_locs[i])

                    if (r$VISUALIZATION == "Explore")
                      umap_to_explore(umap_result) %>% mkdir_saveRDS(f_loc)

                    if (r$VISUALIZATION == "Summarize")
                      umap_to_summary(umap_result) %>% mkdir_saveRDS(f_loc)

                    if (r$VISUALIZATION == "tSNE")
                      umap_to_tsne(umap_result, r$DIMENSION) %>% mkdir_saveRDS(f_loc)
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
                red_update_msg(n_cur, n_fin, rel_fin_locs[i])

                table_to_phate(col_table, com, r$PERPLEXITY) %>% mkdir_saveRDS(f_loc)
              }
            }
          }
        }
      }
    }
  }
}

