# The goal of this script is to convert raw exRNA data
# into numeric data and metadata.

project_name <- "exRNA"
setwd(sprintf("%s/shiny-dim-reduction", Sys.getenv("SHINY_DIM_REDUCTION_ROOT")))
source("aaa_tests/exrna_constants.R", encoding="UTF-8")
source("converter.R", encoding="UTF-8")

# place in a convenient folder to start - try not to jump around too much
setwd(raw_loc)

# -------------------
# IMPORTING FUNCTIONS
# -------------------

# given a list representing a table, remove the preamble
# (such as the exRNA data access policy) and make a data frame
# the preamble is identified by not having at least min_size entries
rem_preamble <- function(tsv_list, min_size)
{
  selected_rows <- sapply(tsv_list, length) > min_size
  shortened <- tsv_list[selected_rows]
  do.call(rbind, shortened) %>% r1_to_cols() %>% data.frame()
}

# removes duplicate rows from a matrix
rem_dupe_rows <- function(data)
{
  data[!duplicated(data),,drop=FALSE]
}

# orders the rows of a matrix by the entries in a column
order_by_col <- function(data, column)
{
  data[order(data[,column]),,drop=FALSE]
}

# combines several functions to make an import pipeline for exRNA
import_pipeline <- function(filenames)
{
  m_list <- my_empty_list(filenames)
  for (filename in filenames)
    m_list[[filename]] <- filename %>% read_tsv_text() %>% rem_preamble(10)
  dplyr::bind_rows(m_list) %>% rem_dupe_rows() %>% order_by_col("FASTQ.IDENTIFIER")
}

# An empty data frame that is well-suited as a placeholder for binds
empty_df <- matrix(0, nrow=1, ncol=1) %>% data.frame()
colnames(empty_df) <- "Unknown"

# ---------------------------
# SPLIT INTO COMMON / URL_LOC
# ---------------------------

bios_ref <- sprintf("Metadata_Raw/Bios_%s.tsv", 1:3) %>% import_pipeline()
expe_ref <- sprintf("Metadata_Raw/Expe_%s.tsv", 1:3) %>% import_pipeline()
dono_ref <- sprintf("Metadata_Raw/Dono_%s.tsv", 1:3) %>% import_pipeline()
rrna_ref <- sprintf("Metadata_Raw/rRNA_%s.tsv", 1:3) %>% import_pipeline()
gene_ref <- sprintf("Metadata_Raw/Gene_%s.tsv", 1:3) %>% import_pipeline()

common_bed <- bios_ref[,c(1, 3:18)]
common_rg <- gene_ref[,c(1, 3:18)]
colnames(common_bed) <- common_cols
colnames(common_rg) <- common_cols

# folders, URLs, and filenames for downloading
dest_folders <- sprintf("Metadata_Cleaned/%s_Mass",
                        c("Bios", "Expe", "Dono", "rRNA", "Gene"))

url_lists <- list(
  "Bios" = bios_ref[,2],
  "Expe" = expe_ref[,2],
  "Dono" = dono_ref[,2],
  "rRNA" = rrna_ref[,2],
  "Gene" = gene_ref[,2]
)

loc_lists <- my_empty_list(c("Bios", "Expe", "Dono", "rRNA", "Gene"))

for (i in 1:5)
{
  # make destination folders
  if (!dir.exists(dest_folders[i]))
    dir.create(dest_folders[i])
  # determine download locations
  range <- 1:length(url_lists[[i]])
  loc_lists[[i]] <- sprintf("%s/%s/M%s.txt", raw_loc, dest_folders[i], range)
}

rm(range)
rm(bios_ref, expe_ref, dono_ref, rrna_ref, gene_ref)

# -------------------
# DOWNLOAD - FRAGILE!
# -------------------

download_status <- my_empty_list(c("Bios", "Expe", "Dono", "rRNA", "Gene"))

# first download - batches of 100
for (i in 1:5)
  download_status[[i]] <- mass_download(url_lists[[i]], loc_lists[[i]], 100)

self_save("download_status")
self_load("download_status")

# second download - batches of 1 and catch missing entries
for (i in 1:5)
{
  urls <- url_lists[[i]]
  locs <- loc_lists[[i]]
  miss <- download_status[[i]][-1]
  mass_download(urls[miss], locs[miss], 1)

  for (j in miss)
    if (file.exists(locs[miss]))
      download_status[[i]] <- setdiff(download_status[[i]], j)
}

self_save(c("common_bed", "common_rg"))

rm(urls, locs, url_lists, loc_lists)

# ----------------
# BIOS, EXPE, DONO
# ----------------

bed_txt <- my_empty_list(c("Bios", "Expe", "Dono"))
self_load("common_bed")

for (i in 1:3)
{
  print(sprintf("i: %s", i))
  final_len <- nrow(common_bed)
  filenames <- sprintf("M%s.txt", 1:final_len)
  bed_txt[[i]] <- my_empty_list(filenames)
  file_locs <- sprintf("%s/%s/%s", raw_loc, dest_folders[i], filenames)

  for (j in 1:final_len)
  {
    if (j %% 1000 == 0)
      print(sprintf("j: %s", j))
    bed_txt[[i]][[j]] <- do.call(rbind, read_tsv_text(file_locs[j]))
  }
}

# takes read_tsv_text from bed_txt and assembles it into a metadata df
bed_assemble <- function(bed_res){
  if (is.null(bed_res))
    return(empty_df)

  bed_res[,1] <- make.unique(bed_res[,1])

  bed_res %>% t() %>% r1_to_cols() %>% data.frame()
}

bios_total <- lapply(bed_txt[[1]], bed_assemble) %>% dplyr::bind_rows()
expe_total <- lapply(bed_txt[[2]], bed_assemble) %>% dplyr::bind_rows()
dono_total <- lapply(bed_txt[[3]], bed_assemble) %>% dplyr::bind_rows()

check_garbo <- function(data, min) {
  select_if(data, function(x){frac_acceptable(x) > min})
}

indices <- which(common_bed$PROFILING != "qPCR")
bios_c <- check_garbo(bios_total[indices,], 0.25)
expe_c <- check_garbo(expe_total[indices,], 0.25)
dono_c <- check_garbo(dono_total[indices,], 0.25)

# dono
make_age_range <- function(x, scale){
  num <- floor(as.numeric(x)/scale)*scale
  sprintf("%s to %s", num, num+scale)
}

dono_clean <- dono_c[,1:6]
dono_clean[is.na(dono_clean)] <- "Unknown"
dono_clean[which(dono_clean == "")] <- "Unknown"
dono_clean <- dono_clean[,-1]
dono_clean[,5] <- gsub(" years", "", dono_clean[,5])
dono_clean[,5] <- gsub(" y", "", dono_clean[,5])
dono_clean <- cbind.data.frame(dono_clean, make_age_range(dono_clean[,5], 5))
dono_clean <- cbind.data.frame(dono_clean, make_age_range(dono_clean[,5], 10))
dono_clean <- dono_clean[,-5]
colnames(dono_clean) <- c("DONOR", "DONOR_TYPE", "SEX", "STATUS",
                          "AGE_5", "AGE_10")

# bios
bios_clean <- bios_c
bios_clean[is.na(bios_clean)] <- "Unknown"
bios_clean[which(bios_clean == "")] <- "Unknown"
bios_clean <- bios_clean[,c(2, 20),drop=FALSE]
colnames(bios_clean) <- c("BIOSAMPLE", "FRACTIONATION")

# expe
expe_clean <- expe_c
expe_clean[is.na(expe_clean)] <- "Unknown"
expe_clean[which(expe_clean == "")] <- "Unknown"

expe_useless <- c(
  "X.property", "X..Status", "X..exRNA.Source.Isolation.Protocol",
  "X...Protocol.Description", "exRNA.Sample.Preparation.Protocol",
  "X..Schema.Version", "X...Biofluid", "X....Description", "X...Protocol.Description.1",
  "X...smRNA.Seq", "X..Experiment.Type", "X....Other.exRNA.Quantification.Method",
  "X......Low.Speed.Centrifugation", "X....Library.Generation",
  "X......Other.Library.Construction.Kit", "X.....Other.Kits",
  "X...RNA.Isolation.Method"
)

for (term in expe_useless)
  expe_clean[[term]] <- NULL

colnames(expe_clean) <- c(
  "EXPERIMENT", "CELL_REMOVAL",  "EXRNA_PREPARATION_PROTOCOL", "ENZYMATIC_TREATMENT",
  "EXRNA_QUANTIFICATION_METHOD","RNA_ISOLATION_KIT", "PROTEINASE_K", "DNASE",
  "RNA_INTEGRITY_METHOD",
  "EXTRACELLULAR_VESICLE_PURIFICATION", "CELL_REMOVAL_METHOD",
  "CENTRIFUGATION_PARAMETERS",
  "CENTRIFUGATION_TEMPERATURE", "CENTRIFUGATION_SPEED", "CENTRIFUGATION_DURATION",
  "AMPLIFIED", "DNA_QUANTIFICATION_METHOD",
  "LIBRARY_CONSTRUCTION_KIT",  "SAMPLES_MULTIPLEXED",
  "STRAND_SPECIFICITY", "STARTING_MATERIAL_TYPE")

# note: redundancy between columns and missing data have been fixed
# keep all factors, even if length(unique) < 2 or length(unique) > num_filters
metadata <- cbind(common_bed[indices,], bios_clean, expe_clean, dono_clean)

metadata$BIOSAMPLE <- NULL
metadata$RNA_ISOLATION_KIT <- NULL

metadata$CONDITION <- rep_str(
  metadata$CONDITION,
  c("Chronic Maternal Hypertension with Superimposed Preeclampsia",
    "Gastric Cancer Pathologic TNM Finding v7",
    "Fetus Small for Gestational Age",
    "Intraventricular Brain Hemorrhage",
    "Transplanted kidney present",
    "Transplanted liver present"),
  c("Maternal Hypertension",
    "Gastric Cancer",
    "Underdeveloped Fetus",
    "Brain Hemorrhage",
    "Kidney Transplant",
    "Liver Transplant")
)

metadata$BIOFLUID <- rep_str(
  metadata$BIOFLUID,
  c("Bronchoalveolar lavage fluid sample",
    "Culture Media, Conditioned",
    "Stool specimen",
    "Ovarian Follicle Fluid"),
  c("Bronchoalveolar",
    "Culture Media",
    "Stool",
    "Ovarian Follicle")
)

metadata$ANATOMICAL <- rep_str(
  metadata$ANATOMICAL,
  c("Brain and spinal cord structure",
    "Entire cardiovascular system",
    "Entire lower respiratory tract",
    "Entire brain",
    "Urinary system structure",
    "Entire "),
  c("Brain/SC Structure",
    "Cardiovascular",
    "Lower Respiratory",
    "Whole Brain",
    "Urinary",
    "")
)

metadata$RNA_KIT <- rep_str(
  metadata$RNA_KIT,
  c("miRCURY RNA isolation kit - Cell & Plant (Exiqon)",
    "MiRVana Paris (Ambion)",
    "SeraMir Kit (System Biosciences)",
    "Trizol + alcohol precipitation",
    "Serum and Plasma kit (Qiagen)",
    "miRcury biofluids (Exiqon)",
    "miRNeasy (Qiagen)"),
  c("miRCURY RNA Isolation, Exiqon",
    "mirVana PARIS, Ambion",
    "SeraMir, System Biosciences",
    "Trizol, Alcohol Precipitation",
    "Serum and Plasma, Qiagen",
    "miRCURY Biofluids, Exiqon",
    "miRNeasy, Qiagen")
)

metadata$RNA_SOURCE <- rep_str(
  metadata$RNA_SOURCE,
  "HDL-containing protein-lipid-RNA complex",
  "HDL-protein-lipid-RNA complex"
)

metadata$BIO_ID <- rep_str(
  metadata$BIO_ID,
  c("EXR-", "-BS"),
  c("", "")
)

metadata$EXPERIMENT <- rep_str(
  metadata$EXPERIMENT,
  c("EXR-", "-EX"),
  c("", "")
)

metadata$DATASET <- rep_str(
  metadata$DATASET,
  c("EXR-", "-AN"),
  c("", "")
)

metadata$DONOR <- rep_str(
  metadata$DONOR,
  c("EXR-", "-DO"),
  c("", "")
)

metadata$LIBRARY_CONSTRUCTION_KIT <- rep_str(
  metadata$LIBRARY_CONSTRUCTION_KIT,
  c("TruSeq Small RNA library prep kit (Illumina)",
    "NEBNext small RNA library prep (NEB)",
    "BioO NEXTFlex kit (V2)",
    "NEXTflex (Bioo)"),
  c("TruSeq Small RNA, Illumina",
    "NEBNext small RNA, NEB",
    "BioO NEXTFlex V2",
    "BioO NEXTFlex V1")
)

metadata$SEX <- rep_str(
  metadata$SEX,
  c("Gender unknown", "Gender unspecified", "female", "male",
    "Masculine gender", "FeMale"),
  c("Unknown", "Unknown", "Female", "Male", "Male", "Female")
)

metadata$CENTRIFUGATION_PARAMETERS <- rep_str(
  metadata$CENTRIFUGATION_PARAMETERS,
  c("5 minutes in 4 C room at 2,000 x g",
    "2500 g x 15 minutes, room temperature",
    "2,000xg for 10 minutes",
    "1500g x 10 minutes and 2500g x 10 minutes",
    "500g x 10 minutes and 2500g x 10 minutes",
    "2600g x 15 minutes",
    "1200g x 10 minutes",
    "2500g x 10 minutes",
    "3000g x 10 minutes",
    "1200 g x 10 minutes",
    "1000g x 10 minutes",
    "1940g x 10 minutes"),
  c("5 min at 4C at 2000g",
    "15 min at RT at 2500g",
    "10 min at 2000g",
    "10 min at 1500g, 10 min at 2500g",
    "10 min at 500g, 10 min at 2500g",
    "15 min at 2600g",
    "10 min at 1200g",
    "10 min at 2500g",
    "10 min at 3000g",
    "10 min at 1200g",
    "10 min at 1000g",
    "10 min at 1940g"
  )
)

rownames(metadata) <- NULL

setwd(raw_loc)
saveRDS(metadata, "bed_metadata.rds")

# ---------------------
# TAXONOMY (Gene, rRNA)
# ---------------------
rg_txt <- my_empty_list(c("rRNA", "Gene"))

for (i in 4:5)
{
  print(sprintf("i: %s", i))
  final_len <- nrow(common_rg)
  filenames <- sprintf("M%s.txt", 1:final_len)
  rg_txt[[i-3]] <- my_empty_list(filenames)
  file_locs <- sprintf("%s/%s/%s", raw_loc, dest_folders[i], filenames)

  for (j in 1:final_len)
  {
    if (j %% 1000 == 0)
      print(sprintf("j: %s", j))
    rg_txt[[i-3]][[j]] <- do.call(rbind, read_tsv_text(file_locs[j]))
  }
}

for (i in 1:2)
  for (j in 1:length(rg_txt[[i]]))
    rg_txt[[i]][[j]] <- rg_txt[[i]][[j]] %>% r1_to_cols() %>% data.frame()

num_taxa <- length(taxonomic_ordering)
num_spe <- num_taxa - 3

# converts a list of levels / names into a sparse table
populate_taxa <- function(data)
{
  result <- data.frame(matrix(0, nrow=nrow(data), ncol=num_taxa))
  for (i in 1:nrow(data))
    result[i, taxonomic_ordering == data[i, 1]] <- data[i, 3]
  result
}

# a super-optimized function for populating the tree to species only
species_only <- function(data)
{
  working <- rep(0, num_spe-1)

  for (i in 1:nrow(data))
  {
    if (data[i, num_spe] != 0)
    {
      data[i, 1:(num_spe-1)] <- working
    }
    else
    {
      for (j in (num_spe-1):1)
      {
        if (data[i,j] != 0)
        {
          working[j] <- data[i,j]
          break
        }
        else
        {
          working[j] <- 0
        }
      }
    }
  }

  data[data[,num_spe] != 0,1:num_spe]
}

make_rownames <- function(names, ids)
{
  mapply(function(a, b){sprintf("%s_%s", a, b)}, names, ids)
}

truncate_data <- function(data)
{
  data[data$level != "no rank",c(2,3,5,6)]
}

rg_assoc <- my_empty_list(names(rg_txt))
rg_txt2 <- rg_assoc

for (n in 1:2)
{
  print(n)
  rg_assoc[[n]] <- my_empty_list(names(rg_txt[[n]]))
  rg_txt2[[n]] <- rg_assoc[[n]]
  start <- my_timer()
  for (k in 1:length(rg_txt[[n]]))
  {
    if (k %% 100 == 0)
    {
      print(sprintf("k: %s", k))
      print(sprintf("Average: %s", my_timer(start)/k))
    }

    data <- rg_txt[[n]][[k]]

    if (length(data) < 1 || nrow(data) < 1 || ncol(data) != 7)
    {
      rg_assoc[[n]][[k]] <- empty_df
      rg_txt2[[n]][[k]] <- empty_df
    }
    else
    {
      x <- truncate_data(data)

      if (sum(x$level == "species") < 1 || ncol(x) != 4)
      {
        rg_assoc[[n]][[k]] <- empty_df
        rg_txt2[[n]][[k]] <- empty_df
      }
      else
      {
        hmm2 <- species_only(populate_taxa(x))
        ind1 <- as.numeric(rownames(hmm2))
        readCounts <- x[ind1,,drop=FALSE]
        rownames(readCounts) <- make_rownames(readCounts$name, readCounts$ID)
        rownames(hmm2) <- rownames(readCounts)
        ind2 <- readCounts$readCount_direct > 0

        rg_assoc[[n]][[k]] <- hmm2[ind2,]
        rg_txt2[[n]][[k]] <- t(readCounts[ind2,4,drop=FALSE]) %>% data.frame()
      }
    }
  }
}

# now that all the cleaning is done, save ...
setwd(raw_loc)
saveRDS(rg_txt2, "rg_txt2.rds")
saveRDS(rg_assoc, "rg_assoc.rds")

setwd(raw_loc)
rg_txt2 <- readRDS("rg_txt2.rds")
rg_assoc <- readRDS("rg_assoc.rds")

# find and keep only actual samples
good_indices <- my_empty_list(names(rg_txt2))
for (i in 1:2)
{
  good_indices[[i]] <- numeric(0)
  for (j in 1:length(rg_txt2[[i]]))
  {
    if (!("Unknown" %in% colnames(rg_txt2[[i]][[j]])))
      good_indices[[i]] <- c(good_indices[[i]], j)
  }
}

for (i in 1:2)
{
  rg_txt2[[i]] <- rg_txt2[[i]][good_indices[[i]]]
  rg_assoc[[i]] <- rg_assoc[[i]][good_indices[[i]]]
}

# bind them all together!
all_rRNA <- chunk_bind_rows(rg_txt2[[1]], 50) %>% chunk_bind_rows()
rownames(all_rRNA) <- good_indices[[1]]
assoc_rRNA <- chunk_bind_rows(rg_assoc[[1]], 50) %>% chunk_bind_rows()
assoc_rRNA <- assoc_rRNA[!duplicated(assoc_rRNA$X25),]
all_gene <- chunk_bind_rows(rg_txt2[[2]], 50) %>% chunk_bind_rows()
rownames(all_gene) <- good_indices[[2]]
assoc_gene <- chunk_bind_rows(rg_assoc[[2]], 50) %>% chunk_bind_rows()
assoc_gene <- assoc_gene[!duplicated(assoc_gene$X25),]

# saving
setwd(raw_loc)
saveRDS(all_rRNA, "all_rRNA.rds")
saveRDS(assoc_rRNA, "assoc_rRNA.rds")
saveRDS(all_gene, "all_gene.rds")
saveRDS(assoc_gene, "assoc_gene.rds")

setwd(raw_loc)
all_rRNA <- readRDS("all_rRNA.rds")
assoc_rRNA <- readRDS("assoc_rRNA.rds")
all_gene <- readRDS("all_gene.rds")
assoc_gene <- readRDS("assoc_gene.rds")

# more cleaning
cleanup_fun_2 <- function(data, frac){
  data[,colSums(is.na(data)) < frac*nrow(data), drop=FALSE]
}

all_rRNA_clean <- cleanup_fun_2(all_rRNA, 0.99)
all_gene_clean <- cleanup_fun_2(all_gene, 0.99)
assoc_rRNA_clean <- assoc_rRNA[colnames(all_rRNA) %in% colnames(all_rRNA_clean),]
assoc_gene_clean <- assoc_gene[colnames(all_gene) %in% colnames(all_gene_clean),]

setwd(raw_loc)
saveRDS(all_rRNA_clean, "all_rrna_clean.rds")
saveRDS(assoc_rRNA_clean, "assoc_rrna_clean.rds")
saveRDS(all_gene_clean, "all_gene_clean.rds")
saveRDS(assoc_gene_clean, "assoc_gene_clean.rds")

# --------------
# CLEAN ALL DATA
# --------------

# set your directory to a folder with tgz files ... this will untar all
for (folder in list.files("TGZ"))
{
  if (dir.exists(sprintf("TGZ/%s", folder)))
  {
    for (file in list.files(sprintf("TGZ/%s", folder)))
    {
      result_dir <- sprintf("UNTAR/%s/%s", folder, rep_str(file, ".tgz", ""))
      dir.create(result_dir)
      untar(sprintf("TGZ/%s/%s", folder, file), exdir=result_dir)
    }
  }
}

# move all the .tgz files into Raw and all the folders into Data
# Then run this code to make cleaned versions of all folders

for (folder in list.files("UNTAR"))
{
  if (dir.exists(sprintf("UNTAR/%s", folder)))
  {
    for (archive in list.files(sprintf("UNTAR/%s", folder)))
    {
      subaddr <- sprintf("UNTAR/%s/%s", folder, archive)
      finaddr <- sprintf("CLEAN/%s/%s", folder, archive)
      if (!file.exists(finaddr))
        dir.create(finaddr, recursive=TRUE)

      all_copies <- list.files(subaddr)
      splitup <- strsplit(all_copies, "_exceRpt_", fixed=TRUE)
      actual_names <- unlist(lapply(splitup, function(x){x[2]}))
      initial_data_locs <- sprintf("%s/%s", subaddr, all_copies)
      final_data_locs <- sprintf("%s/%s", finaddr, actual_names)
      file.copy(from = initial_data_locs, to = final_data_locs)
    }
  }
}

# -----------------------
# PROCESS SUMMARY REPORTS
# -----------------------

list_of_fragments <- NULL

for (folder in list.files("UNTAR"))
{
  for (archive in list.files(sprintf("UNTAR/%s", folder)))
  {
    finaddr <- sprintf("CLEAN/%s/%s/smallRNAQuants_ReadsPerMillion.RData",
                       folder, archive)
    if (file.exists(finaddr))
      list_of_fragments <- c(list_of_fragments,finaddr)
  }
}

# function for converting raw to a data frame
raw_to_df <- function(rpm) {
  df <- as.data.frame(t(rpm))
  df <- cbind(rownames(df), data.frame(df, row.names=NULL))
  colnames(df)[1] <- "CORNER_CORNER"
  df
}

# load all data and aggregate it
miRNA <- my_empty_list(sprintf("M%s", 1:length(list_of_fragments)))
piRNA <- miRNA
tRNA <- miRNA
circRNA <- miRNA
ex_miRNA <- miRNA
cumulative_ex_genomes <- miRNA
specific_ex_genomes <- miRNA
cumulative_ex_ribosomes <- miRNA
specific_ex_ribosomes <- miRNA
# gencode <- miRNA

for (i in 1:length(list_of_fragments))
{
  load(list_of_fragments[i])
  miRNA[[i]] <- exprs.miRNA.rpm %>% raw_to_df()
  piRNA[[i]] <- exprs.piRNA.rpm %>% raw_to_df()
  tRNA[[i]] <- exprs.tRNA.rpm %>% raw_to_df()
  circRNA[[i]] <- exprs.circRNA.rpm %>% raw_to_df()
  ex_miRNA[[i]] <- exprs.exogenous_miRNA.rpm %>% raw_to_df()
  cumulative_ex_genomes[[i]] <- exprs.exogenousGenomes_cumulative.rpm %>% raw_to_df()
  specific_ex_genomes[[i]] <- exprs.exogenousGenomes_specific.rpm %>% raw_to_df()
  cumulative_ex_ribosomes[[i]] <- exprs.exogenousRibosomal_cumulative.rpm %>% raw_to_df()
  specific_ex_ribosomes[[i]] <- exprs.exogenousRibosomal_specific.rpm %>% raw_to_df()
  # gencode[[i]] <- exprs.gencode.rpm %>% raw_to_df()
}

rm(
  exprs.miRNA.rpm
  , exprs.piRNA.rpm
  , exprs.tRNA.rpm,exprs.circRNA.rpm
  , exprs.exogenous_miRNA.rpm
  , exprs.exogenousGenomes_cumulative.rpm
  , exprs.exogenousGenomes_specific.rpm
  , exprs.exogenousRibosomal_cumulative.rpm
  , exprs.exogenousRibosomal_specific.rpm
  , exprs.gencode.rpm
)

miRNA <- local_bind(miRNA)
piRNA <- local_bind(piRNA)
tRNA <- local_bind(tRNA)
circRNA <- local_bind(circRNA)
ex_miRNA <- local_bind(ex_miRNA)
cumulative_ex_genomes <- local_bind(cumulative_ex_genomes)
specific_ex_genomes <- local_bind(specific_ex_genomes)
cumulative_ex_ribosomes <- local_bind(cumulative_ex_ribosomes)
specific_ex_ribosomes <- local_bind(specific_ex_ribosomes)
# gencode <- dplyr::bind_rows(gencode)

# remove duplicated FASTQ entries
rem_dup <- function(data){
  data[which(!duplicated(data[,1])),]
}

miRNA <- rem_dup(miRNA)
piRNA <- rem_dup(piRNA)
tRNA <- rem_dup(tRNA)
circRNA <- rem_dup(circRNA)
ex_miRNA <- rem_dup(ex_miRNA)
cumulative_ex_genomes <- rem_dup(cumulative_ex_genomes)
specific_ex_genomes <- rem_dup(specific_ex_genomes)
cumulative_ex_ribosomes <- rem_dup(cumulative_ex_ribosomes)
specific_ex_ribosomes <- rem_dup(specific_ex_ribosomes)
# gencode <- rem_dup(gencode)

setwd(sprintf("%s/Summary_Cleaned", raw_loc))
saveRDS(miRNA, "miRNA.rds")
saveRDS(piRNA, "piRNA.rds")
saveRDS(tRNA, "tRNA.rds")
saveRDS(circRNA, "circRNA.rds")
saveRDS(ex_miRNA, "ex_miRNA.rds")
saveRDS(cumulative_ex_genomes, "cumulative_ex_genomes.rds")
saveRDS(specific_ex_genomes, "specific_ex_genomes.rds")
saveRDS(cumulative_ex_ribosomes, "cumulative_ex_ribosomes.rds")
saveRDS(specific_ex_ribosomes, "specific_ex_ribosomes.rds")
# saveRDS(gencode, "gencode.rds")

# ----------------
# TWO NEW DATASETS
# ----------------

filenames <- list.files("twonewdatasets")
files <- my_empty_list(filenames)

for (file in filenames)
  files[[file]] <- read_tsv_text(sprintf("twonewdatasets/%s", file))

f1 <- files[1:6]

for (file in names(f1))
{
  max_len <- 0
  for (i in 1:length(f1[[file]]))
  {
    entry <- f1[[file]][[i]]
    if (length(entry) > max_len)
      max_len <- length(entry)
    while (length(entry) < max_len)
      entry <- c(entry, " ")
    f1[[file]][[i]] <- entry
  }
  f1[[file]] <- do.call(rbind, f1[[file]]) %>% t() %>% r1_to_cols() %>% data.frame()
}

for (file in names(f1))
{
  f1[[file]] <- f1[[file]][1:(nrow(f1[[file]])-4),]
}

f2 <- files[7:15]

for (file in names(f2))
{
  max_len <- 0
  for (i in 1:length(f2[[file]]))
  {
    entry <- f2[[file]][[i]]
    if (length(entry) > max_len)
      max_len <- length(entry)
    while (length(entry) < max_len)
      entry <- c(entry, " ")
    f2[[file]][[i]] <- entry
  }
  f2[[file]] <- do.call(rbind, f2[[file]]) %>% t() %>% r1_to_cols() %>% data.frame()
}

f11 <- dplyr::bind_rows(f1)
f22 <- dplyr::bind_rows(f2)
rm(f1, f2)

f111 <- f11 %>% check_garbo(0.25)

# dgala fix
hmm <- read_tsv_text("dgala_metadata.tsv")
h2 <- rem_preamble(hmm, 2)
h2[[2]] <- NULL
colnames(h2) <- common_cols
colnames(f22)[2] <- "BIO_ID"

dgala_metadata <- dplyr::full_join(h2, f22, by = "BIO_ID")
colnames(dgala_metadata)[c(19, 24, 25, 53)] <- c(
  "DONOR_TYPE", "DONOR", "AGE", "FRACTIONATION")
dgala_metadata[c(18,20:23,26:52)] <- NULL

ages <- gsub(" years", "", dgala_metadata$AGE)
age5 <- make_age_range(ages, 5)
age10 <- make_age_range(ages, 10)

dgala_metadata$AGE <- NULL
dgala_metadata$AGE_5 <- age5
dgala_metadata$AGE_10 <- age10

bed_metadata_rev <- dplyr::bind_rows(bed_metadata, dgala_metadata)
missing_meta <- setdiff(miRNA[,1], bed_metadata_rev$FASTQ_IDENTIFIER)
saveRDS(bed_metadata_rev, "metadata.rds")

# ---------
# FINISHING
# ---------

# a very cool trick
match_order <- function(fastq_list, ord)
{
  missed <- which(!(fastq_list %in% ord$FASTQ_IDENTIFIER))
  original_len <- nrow(ord)

  for (i in 1:length(missed))
    ord[nrow(ord)+1,] <- "Unknown"

  for (i in 1:length(missed))
    ord$FASTQ_IDENTIFIER[i+original_len] <- fastq_list[missed[i]]

  ord[match(fastq_list, ord$FASTQ_IDENTIFIER),]
}

# another cool trick
cut_features <- function(data, thresh){
  bad_indices <- NULL

  for (j in 2:ncol(data))
  {
    if (sum(is.na(data[[j]])) > thresh*nrow(data))
      bad_indices <- c(bad_indices, j)
  }

  good_indices <- setdiff(1:ncol(data), bad_indices)
  data[,good_indices,drop=FALSE]
}

setwd(raw_loc)
self_load(c("metadata", "all_rrna_clean", "assoc_rrna_clean",
            "all_gene_clean", "assoc_gene_clean"))
rownames(assoc_rrna_clean) <- NULL
colnames(assoc_rrna_clean) <- taxonomic_ordering[1:25]
rownames(assoc_gene_clean) <- NULL
colnames(assoc_gene_clean) <- taxonomic_ordering[1:25]

setwd(sprintf("%s/Summary_Cleaned", raw_loc))
miRNA <- readRDS("miRNA.rds")
piRNA <- readRDS("piRNA.rds")
tRNA <- readRDS("tRNA.rds")
circRNA <- readRDS("circRNA.rds")
ex_miRNA <- readRDS("ex_miRNA.rds")
cumulative_ex_genomes <- readRDS("cumulative_ex_genomes.rds")
specific_ex_genomes <- readRDS("specific_ex_genomes.rds")
cumulative_ex_ribosomes <- readRDS("cumulative_ex_ribosomes.rds")
specific_ex_ribosomes <- readRDS("specific_ex_ribosomes.rds")
# gencode <- readRDS("gencode.rds")

miRNA <- cut_features(miRNA, 0.95)
piRNA <- cut_features(piRNA, 0.95)
tRNA <- cut_features(tRNA, 0.95)
circRNA <- cut_features(circRNA, 0.95)
ex_miRNA <- cut_features(ex_miRNA, 0.95)
cumulative_ex_genomes <- cut_features(cumulative_ex_genomes, 0.05)
specific_ex_genomes <- cut_features(specific_ex_genomes, 0.05)
cumulative_ex_ribosomes <- cut_features(cumulative_ex_ribosomes, 0.05)
specific_ex_ribosomes <- cut_features(specific_ex_ribosomes, 0.05)
# gencode <- cut_features(gencode, 0.05)

assoc_rrna_clean[assoc_rrna_clean == 0] <- "Unknown"
assoc_gene_clean[assoc_gene_clean == 0] <- "Unknown"

order_miRNA <- match_order(miRNA[,1], metadata)
order_piRNA <- match_order(piRNA[,1], metadata)
order_tRNA <- match_order(tRNA[,1], metadata)
order_circRNA <- match_order(circRNA[,1], metadata)
order_ex_miRNA <- match_order(ex_miRNA[,1], metadata)
order_cumulative_ex_genomes <- match_order(cumulative_ex_genomes[,1], metadata)
order_specific_ex_genomes <- match_order(specific_ex_genomes[,1], metadata)
order_cumulative_ex_ribosomes <- match_order(cumulative_ex_ribosomes[,1], metadata)
order_specific_ex_ribosomes <- match_order(specific_ex_ribosomes[,1], metadata)
order_rRNA_Species <- metadata[rownames(all_rrna_clean),]
order_Gene_Species <- metadata[rownames(all_gene_clean),]
order_rRNA_Transpose <- assoc_rrna_clean
order_Gene_Transpose <- assoc_gene_clean

order_total <- list()
for (cat in name_cat)
  order_total[[cat]] <- get(sprintf("order_%s", cat))

setwd(dep_loc)
saveRDS(categories_full, "categories_full.rds")
saveRDS(order_total, "order_total.rds")

miRNA <- miRNA[,-1]
piRNA <- piRNA[,-1]
tRNA <- tRNA[,-1]
circRNA <- circRNA[,-1]
ex_miRNA <- ex_miRNA[,-1]
cumulative_ex_genomes <- cumulative_ex_genomes[,-1]
specific_ex_genomes <- specific_ex_genomes[,-1]
cumulative_ex_ribosomes <- cumulative_ex_ribosomes[,-1]
specific_ex_ribosomes <- specific_ex_ribosomes[,-1]
# gencode <- gencode[,-1]
rRNA_species <- all_rrna_clean
gene_species <- all_gene_clean
rRNA_transpose <- t(all_rrna_clean)
gene_transpose <- t(all_gene_clean)
rownames(rRNA_species) <- NULL
rownames(gene_species) <- NULL
rownames(rRNA_transpose) <- NULL
rownames(gene_transpose) <- NULL
colnames(rRNA_transpose) <- order_total$rRNA_Species$FASTQ_IDENTIFIER
colnames(gene_transpose) <- order_total$Gene_Species$FASTQ_IDENTIFIER

combined_miRNA <- convert_to_num(miRNA)
combined_piRNA <- convert_to_num(piRNA)
combined_tRNA <- convert_to_num(tRNA)
combined_circRNA <- convert_to_num(circRNA)
combined_ex_miRNA <- convert_to_num(ex_miRNA)
combined_cumulative_ex_genomes <- convert_to_num(cumulative_ex_genomes)
combined_specific_ex_genomes <- convert_to_num(specific_ex_genomes)
combined_cumulative_ex_ribosomes <- convert_to_num(cumulative_ex_ribosomes)
combined_specific_ex_ribosomes <- convert_to_num(specific_ex_ribosomes)
# combined_gencode <- convert_to_num(gencode)
combined_rRNA_species <- convert_to_num(rRNA_species)
combined_gene_species <- convert_to_num(gene_species)
combined_rRNA_transpose <- convert_to_num(rRNA_transpose)
combined_gene_transpose <- convert_to_num(gene_transpose)

# ---------
# SAVE DATA
# ---------

setwd(pro_loc)
saveRDS(combined_miRNA, "combined/combined_miRNA.rds")
saveRDS(combined_piRNA, "combined/combined_piRNA.rds")
saveRDS(combined_tRNA, "combined/combined_tRNA.rds")
saveRDS(combined_circRNA, "combined/combined_circRNA.rds")
saveRDS(combined_ex_miRNA, "combined/combined_ex_miRNA.rds")
saveRDS(combined_cumulative_ex_genomes, "combined/combined_cumulative_ex_genomes.rds")
saveRDS(combined_specific_ex_genomes, "combined/combined_specific_ex_genomes.rds")
saveRDS(combined_cumulative_ex_ribosomes, "combined/combined_cumulative_ex_ribosomes.rds")
saveRDS(combined_specific_ex_ribosomes, "combined/combined_specific_ex_ribosomes.rds")
saveRDS(combined_rRNA_species, "combined/combined_rRNA_Species.rds")
saveRDS(combined_gene_species, "combined/combined_Gene_Species.rds")
saveRDS(combined_rRNA_transpose, "combined/combined_rRNA_Transpose.rds")
saveRDS(combined_gene_transpose, "combined/combined_Gene_Transpose.rds")

# -----------------
# SAVE DEPENDENCIES
# -----------------

metadata[is.na(metadata)] <- "Unknown"

setwd(pro_loc)
decorations <- list()
init_cat()

ind_sd_top_1000 <- function(data)
{
  vals <- apply(data, 2, sd)
  order(vals)[1:1000]
}

for (cat in name_cat)
{
  combined <- readRDS(sprintf("combined/combined_%s.rds", cat))
  print(dim(combined))
  decorations[[cat]] <- list(
    "Categories" = cat,
    "Subsets"=list(
      "Reference" = colnames(combined),
      "SD_Top_1000" = ind_sd_top_1000(combined)
    )
  )
}

decorations$tRNA <- NULL

setwd(dep_loc)

for (i in 1:num_cat)
  rownames(order_total[[i]]) <- NULL

amazon_keys <- list("id" = "AKIAVI2HZGPODUFE62HE",
                    "secret" = "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",
                    "bucket" = "shiny-app-data-justin-exrna")
perplexity_types <- c(10, 20, 30, 50, 100)
pc_cap <- 10
user_credentials <- list("guest"=my_hash("All@2019"))

self_save(c("amazon_keys", "app_title", "app_citations", "perplexity_types",
          "pc_cap", "user_credentials", "decorations"))
