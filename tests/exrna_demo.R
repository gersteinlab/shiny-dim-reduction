# The goal of this script is to convert raw exRNA data
# into numeric data and metadata.

if (!exists("sdr_config") || sdr_config$mode != "pipeline")
  source("app/install.R")
stopifnot(sdr_config$mode == "pipeline")

library(data.table)

source("tests/exrna_converter.R")
source("app/app_utils.R")
source("app/storage.R")
source("pipeline/workflows.R")

# upsert_workflow("exRNA", "C:/Users/justin/Desktop/CodeR/DataR/sdr_workflows")
# save_wf_config()
load_wf_config()
set_workflow("exRNA")

# ---------------------
# CONSTANT DEPENDENCIES
# ---------------------

app_title <- "Dimensionality Reduction Plotting Tool for the exRNA Atlas"

app_citations <- "<u>ERCC:</u>
Ainsztein AM, Brooks PJ, Dugan VG, et al. The NIH Extracellular RNA Communication
Consortium. J Extracell Vesicles. 2015;4:27493. Published 2015 Aug 28.
<a href=\"doi:10.3402/jev.v4.27493\" target=\"_blank\">
doi:10.3402/jev.v4.27493</a>
<br>
<u>exceRpt Tool:</u> Rozowsky J, Kitchen RR, Park JJ, et al.
exceRpt: A Comprehensive Analytic Platform for Extracellular RNA Profiling.
Cell Syst. 2019;8(4):352-357.e3.
<a href=\"doi:10.1016/j.cels.2019.03.004\" target=\"_blank\">
doi:10.1016/j.cels.2019.03.004</a>
<br>
<u>exRNA Atlas:</u> Murillo OD, Thistlethwaite W, Rozowsky J, et al.
exRNA Atlas Analysis Reveals Distinct Extracellular RNA Cargo Types and
Their Carriers Present across Human Biofluids. Cell. 2019;177(2):463-477.e15.
<a href=\"doi:10.1016/j.cell.2019.02.018\" target=\"_blank\">
doi:10.1016/j.cell.2019.02.018</a>
<br><br>
In addition, the NIH Common Fund, ERCC, and many ERCC producers
graciously generated these datasets."

set_dependency("amazon_keys")
set_dependency("app_title")
set_dependency("app_citations")
set_dependency("user_credentials")
# note: custom color scales not used here

# ----------------
# IMPORT CONSTANTS
# ----------------

# a predetermined ordering for taxonomy
taxonomic_ordering <- c(
  "superkingdom", "kingdom", "subkingdom",
  "superphylum", "phylum", "subphylum",
  "superclass", "class", "subclass", "infraclass",
  "superorder", "order", "suborder", "infraorder", "parvorder",
  "superfamily", "family", "subfamily",
  "tribe", "subtribe",
  "genus", "subgenus",
  "species group", "species subgroup",
  "species", "subspecies",
  "varietas", "forma"
)

taxa_ordering_25 <- taxonomic_ordering[1:25]

num_taxa <- length(taxonomic_ordering)
num_spe <- num_taxa - 3

# common metadata columns
common_cols <- c(
  "DOWNLOAD_NAME", "BIO_NAME", "CONDITION", "BIOFLUID",
  "RNA_SOURCE", "RNA_KIT", "ANATOMICAL", "CELL_SOURCE", "PROFILING",
  "SPECIES", "STANDARDS", "TRANSCRIPTOME", "REFERENCE", "RATIO",
  "BIO_ID", "DATASET", "FASTQ_IDENTIFIER")

status_loc <- sprintf("%s/Status", raw_loc)

# -------------------
# IMPORTING FUNCTIONS
# -------------------

# combines several functions to make an import pipeline for exRNA
import_pipeline <- function(filenames)
{
  m_list <- empty_named_list(filenames)
  for (filename in filenames)
    m_list[[filename]] <- filename %>% read_tsv_text() %>% rem_preamble(10)
  dplyr::bind_rows(m_list) %>% rem_dupe_rows() %>% order_by_col("FASTQ.IDENTIFIER")
}

# ---------------------------
# SPLIT INTO COMMON / URL_LOC
# ---------------------------

# folders, URLs, and filenames for downloading
raw_meta_loc <- sprintf("%s/Metadata_Raw", raw_loc)
meta_five_types <- c("Bios", "Expe", "Dono", "rRNA", "Gene")
dest_folders <- sprintf("%s/%s_Mass", raw_meta_loc, meta_five_types)

bios_ref <- sprintf("%s/Bios_%s.tsv", raw_meta_loc, 1:3) %>% import_pipeline()
expe_ref <- sprintf("%s/Expe_%s.tsv", raw_meta_loc, 1:3) %>% import_pipeline()
dono_ref <- sprintf("%s/Dono_%s.tsv", raw_meta_loc, 1:3) %>% import_pipeline()
rrna_ref <- sprintf("%s/rRNA_%s.tsv", raw_meta_loc, 1:3) %>% import_pipeline()
gene_ref <- sprintf("%s/Gene_%s.tsv", raw_meta_loc, 1:3) %>% import_pipeline()

non_download_cols <- c(1, 3:18)
common_bed <- bios_ref[,non_download_cols]
common_rg <- gene_ref[,non_download_cols]
colnames(common_bed) <- common_cols
colnames(common_rg) <- common_cols

# the list of all URLs for each type
url_lists <- list(bios_ref[,2], expe_ref[,2], dono_ref[,2], rrna_ref[,2], gene_ref[,2])
# the list of all locations for each URL download
loc_lists <- empty_named_list(meta_five_types)

for (i in 1:5)
{
  # ensure dest folders exist
  safe_dir(dest_folders[i])
  # determine download locations
  loc_lists[[i]] <- sprintf("%s/M%s.txt", dest_folders[i], seq_along(url_lists[[i]]))
}

rm(bios_ref, expe_ref, dono_ref, rrna_ref, gene_ref)

# -------------------
# DOWNLOAD - FRAGILE!
# -------------------

download_status <- empty_named_list(meta_five_types)

# first download - batches of 100
for (i in 1:5)
  download_status[[i]] <- mass_download(url_lists[[i]], loc_lists[[i]], 100)

set_self_rds("download_status", status_loc)
get_self_rds("download_status", status_loc)

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

set_self_rds("common_bed", status_loc)
set_self_rds("common_rg", status_loc)

rm(urls, locs, url_lists, loc_lists)

# ----------------
# BIOS, EXPE, DONO
# ----------------

get_self_rds("common_bed", status_loc)
bed_txt <- empty_named_list(c("Bios", "Expe", "Dono"))

for (i in 1:3)
{
  print(sprintf("i: %s", i))
  file_locs <- sprintf("%s/M%s.txt", dest_folders[i], seq_len(nrow(common_bed)))
  bed_txt[[i]] <- tsv_files_to_matrix(file_locs)
}

bios_total <- transpose_to_df_multi(bed_txt[[1]])
expe_total <- transpose_to_df_multi(bed_txt[[2]])
dono_total <- transpose_to_df_multi(bed_txt[[3]])

check_garbo <- function(data, min) {
  select_if(data, function(x){frac_acceptable(x) > min})
}

indices <- which(common_bed$PROFILING != "qPCR")
bios_c <- check_garbo(bios_total[indices,], 0.25)
expe_c <- check_garbo(expe_total[indices,], 0.25)
dono_c <- check_garbo(dono_total[indices,], 0.25)

# dono
dono_clean <- dono_c[,1:6]
dono_clean[is.na(dono_clean)] <- "Unknown"
dono_clean[which(dono_clean == "")] <- "Unknown"
dono_clean <- dono_clean[,-1]
dono_clean[,5] <- gsub(" years", "", dono_clean[,5])
dono_clean[,5] <- gsub(" y", "", dono_clean[,5])
dono_clean <- cbind.data.frame(dono_clean, get_intervals(dono_clean[,5], 5))
dono_clean <- cbind.data.frame(dono_clean, get_intervals(dono_clean[,5], 10))
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

# ----------------------------
# TAXONOMY (Gene, rRNA) PART 1
# ----------------------------

get_self_rds("common_rg", status_loc)
rg_txt <- empty_named_list(c("rRNA", "Gene"))

for (i in 4:5)
{
  print(sprintf("i: %s", i))
  file_locs <- sprintf("%s/M%s.txt", dest_folders[i], seq_len(nrow(common_rg)))
  bed_txt[[i-3]] <- tsv_files_to_matrix(file_locs)

}

for (i in 1:2)
  for (j in seq_along(rg_txt[[i]]))
    rg_txt[[i]][[j]] <- simple_mat_to_df(rg_txt[[i]][[j]])

rg_assoc <- empty_named_list(names(rg_txt))
rg_txt2 <- rg_assoc

for (n in 1:2)
{
  print(n)
  rg_assoc[[n]] <- empty_named_list(names(rg_txt[[n]]))
  rg_txt2[[n]] <- rg_assoc[[n]]
  start <- my_timer()
  for (k in seq_along(rg_txt[[n]]))
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
        rownames(readCounts) <- sprintf("%s_%s", readCounts$name, readCounts$ID)
        rownames(hmm2) <- rownames(readCounts)
        ind2 <- readCounts$readCount_direct > 0

        rg_assoc[[n]][[k]] <- hmm2[ind2,]
        rg_txt2[[n]][[k]] <- t(readCounts[ind2,4,drop=FALSE]) %>% data.frame()
      }
    }
  }
}

# now that all the cleaning is done, save ...
# set_self_rds("rg_txt2", status_loc)
# set_self_rds("rg_assoc", status_loc)

get_self_rds("rg_txt2", status_loc)
get_self_rds("rg_assoc", status_loc)
good_indices <- empty_named_list(names(rg_txt2))

for (i in 1:2)
{
  good_indices[[i]] <- get_known_df_indices(rg_txt2[[i]])
  rg_txt2[[i]] <- rg_txt2[[i]][good_indices[[i]]]
  rg_assoc[[i]] <- rg_assoc[[i]][good_indices[[i]]]
}

# bind them all together!
all_rRNA <- local_bind(rg_txt2[[1]], 50)
rownames(all_rRNA) <- good_indices[[1]]

assoc_rRNA <- local_bind(rg_assoc[[1]], 50)
colnames(assoc_rRNA) <- taxa_ordering_25
assoc_rRNA <- assoc_rRNA[!duplicated(assoc_rRNA$species),]

all_gene <- chunk_bind_rows(rg_txt2[[2]], 50) %>% chunk_bind_rows()
rownames(all_gene) <- good_indices[[2]]

assoc_gene <- chunk_bind_rows(rg_assoc[[2]], 50) %>% chunk_bind_rows()
colnames(assoc_gene) <- taxa_ordering_25
assoc_gene <- assoc_gene[!duplicated(assoc_gene$species),]

# saving
set_self_rds("all_rRNA", status_loc)
set_self_rds("assoc_rRNA", status_loc)
set_self_rds("all_gene", status_loc)
set_self_rds("assoc_gene", status_loc)

# ----------------------------
# TAXONOMY (Gene, rRNA) PART 2
# ----------------------------

# loading
get_self_rds("all_rRNA", status_loc)
get_self_rds("assoc_rRNA", status_loc)
get_self_rds("all_gene", status_loc)
get_self_rds("assoc_gene", status_loc)

# more cleaning
cleanup_fun_2 <- function(data, frac){
  data[,colSums(is.na(data)) < frac*nrow(data), drop=FALSE]
}

clean_all_rRNA <- cleanup_fun_2(all_rRNA, 0.99)
clean_all_gene <- cleanup_fun_2(all_gene, 0.99)

clean_assoc_rRNA <- assoc_rRNA[colnames(all_rRNA) %in% colnames(clean_all_rRNA),]
rownames(clean_assoc_rRNA) <- NULL
clean_assoc_rRNA[clean_assoc_rRNA == 0] <- "Unknown"
clean_assoc_gene <- assoc_gene[colnames(all_gene) %in% colnames(clean_all_gene),]
rownames(clean_assoc_gene) <- NULL
clean_assoc_gene[clean_assoc_gene == 0] <- "Unknown"

set_self_rds("clean_all_rRNA", status_loc)
set_self_rds("clean_assoc_rRNA", status_loc)
set_self_rds("clean_all_gene", status_loc)
set_self_rds("clean_assoc_gene", status_loc)

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
miRNA <- empty_named_list(sprintf("M%s", 1:length(list_of_fragments)))
piRNA <- miRNA
tRNA <- miRNA
circRNA <- miRNA
ex_miRNA <- miRNA
cumulative_ex_genomes <- miRNA
specific_ex_genomes <- miRNA
cumulative_ex_ribosomes <- miRNA
specific_ex_ribosomes <- miRNA
# gencode <- miRNA

for (i in seq_along(list_of_fragments))
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
files <- empty_named_list(filenames)

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
age5 <- get_intervals(ages, 5)
age10 <- get_intervals(ages, 10)

dgala_metadata$AGE <- NULL
dgala_metadata$AGE_5 <- age5
dgala_metadata$AGE_10 <- age10

bed_metadata_rev <- dplyr::bind_rows(bed_metadata, dgala_metadata)
missing_meta <- setdiff(miRNA[,1], bed_metadata_rev$FASTQ_IDENTIFIER)

setwd(raw_loc)
saveRDS(bed_metadata_rev, "metadata.rds")

# ---------
# FINISHING
# ---------

get_self_rds("clean_all_rRNA", status_loc)
get_self_rds("clean_assoc_rRNA", status_loc)
get_self_rds("clean_all_gene", status_loc)
get_self_rds("clean_assoc_gene", status_loc)
get_self_rds("metadata", raw_loc)

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

miRNA <- remove_excess_na_cols(miRNA, 0.95)
piRNA <- remove_excess_na_cols(piRNA, 0.95)
tRNA <- remove_excess_na_cols(tRNA, 0.95)
circRNA <- remove_excess_na_cols(circRNA, 0.95)
ex_miRNA <- remove_excess_na_cols(ex_miRNA, 0.95)
cumulative_ex_genomes <- remove_excess_na_cols(cumulative_ex_genomes, 0.05)
specific_ex_genomes <- remove_excess_na_cols(specific_ex_genomes, 0.05)
cumulative_ex_ribosomes <- remove_excess_na_cols(cumulative_ex_ribosomes, 0.05)
specific_ex_ribosomes <- remove_excess_na_cols(specific_ex_ribosomes, 0.05)
# gencode <- remove_excess_na_cols(gencode, 0.05)

order_miRNA <- match_metadata_to_samples(miRNA[,1], metadata)
order_piRNA <- match_metadata_to_samples(piRNA[,1], metadata)
order_tRNA <- match_metadata_to_samples(tRNA[,1], metadata)
order_circRNA <- match_metadata_to_samples(circRNA[,1], metadata)
order_ex_miRNA <- match_metadata_to_samples(ex_miRNA[,1], metadata)
order_cumulative_ex_genomes <- match_metadata_to_samples(cumulative_ex_genomes[,1], metadata)
order_specific_ex_genomes <- match_metadata_to_samples(specific_ex_genomes[,1], metadata)
order_cumulative_ex_ribosomes <- match_metadata_to_samples(cumulative_ex_ribosomes[,1], metadata)
order_specific_ex_ribosomes <- match_metadata_to_samples(specific_ex_ribosomes[,1], metadata)
order_rRNA_Species <- metadata[rownames(clean_assoc_rRNA),]
order_Gene_Species <- metadata[rownames(clean_assoc_Gene),]
order_rRNA_Transpose <- clean_assoc_rRNA
order_Gene_Transpose <- clean_assoc_gene

order_total <- list()
for (cat in name_cat)
  order_total[[cat]] <- get(sprintf("order_%s", cat))

set_dependency("order_total")

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
write.csv(combined_miRNA, "combined_miRNA_unfiltered.csv")
write.csv(combined_piRNA, "combined_piRNA_unfiltered.csv")
write.csv(combined_tRNA, "combined_tRNA_unfiltered.csv")
write.csv(combined_circRNA, "combined_circRNA_unfiltered.csv")
write.csv(combined_ex_miRNA, "combined_ex_miRNA_unfiltered.csv")
write.csv(combined_cumulative_ex_genomes, "combined_cumulative_ex_genomes_unfiltered.csv")
write.csv(combined_specific_ex_genomes, "combined_specific_ex_genomes_unfiltered.csv")
write.csv(combined_cumulative_ex_ribosomes, "combined_cumulative_ex_ribosomes_unfiltered.csv")
write.csv(combined_specific_ex_ribosomes, "combined_specific_ex_ribosomes_unfiltered.csv")

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

setwd(pro_loc)

metadata[is.na(metadata)] <- "Unknown"

setwd(pro_loc)
decorations <- list()
init_cat()

make_row_subsets <- function(metadata)
{
  row_subsets <- list()

  for (cha in c("BIOFLUID", "DATASET"))
  {
    if (cha %in% names(metadata))
    {
      for (bio_type in unique(metadata[[cha]]))
      {
        order_logi <- (metadata[[cha]] == bio_type)
        if (sum(order_logi) >= 100)
          row_subsets[[bio_type]] <- which(order_logi)
      }
    }
  }

  row_subsets
}

for (cat in name_cat)
{
  combined <- readRDS(sprintf("combined/combined_%s.rds", cat))
  stopifnot(length(colnames(combined)) == ncol(combined))
  # rownames(combined) <- NULL
  # saveRDS(combined, sprintf("combined/combined_%s.rds", cat))
  print(dim(combined))

  decorations[[cat]] <- list(
    "CATEGORIES" = cat,
    "ROW_SUBSETS" = list(),
    "COL_SUBSETS"=list("Reference" = colnames(combined))
  )

  for (cha in c("BIOFLUID", "DATASET"))
  {
    if (cha %in% names(order_total[[cat]]))
    {
      for (bio_type in unique(order_total[[cat]][[cha]]))
      {
        indices <- which(order_total[[cat]][[cha]] == bio_type)

        # if there are at least 100 samples with this label
        if (length(indices) >= 100)
          decorations[[cat]][["ROW_SUBSETS"]][[bio_type]] <- indices
      }
    }
  }

  if (ncol(combined) > 1000)
  {
    decorations[[cat]][["COL_SUBSETS"]][["SD_Top_1000"]] = ind_sd_top(combined, 1000)
    decorations[[cat]][["COL_SUBSETS"]][["SD_Top_100"]] = ind_sd_top(combined, 100)
  }
}

setwd(dep_loc)

for (i in 1:num_cat)
  rownames(order_total[[i]]) <- NULL

self_save(c("amazon_keys", "app_title", "app_citations", "perplexity_types",
            "pc_cap", "user_credentials", "decorations"))

# -----------------
# RBP INTERSECTIONS
# -----------------

setwd(wf_config$raw_loc)
tgz_files <- list.files("RBP_raw")
num_archives <- length(tgz_files)
# dir.create("RBP_S1") # stage 1: untar

rbp_dirs <- character(num_archives)
rbp_xzs <- list()

# untar and check that all work
for (i in 1:num_archives)
{
  dest_dir <- sprintf("RBP_S1/a%s", i)
  untar(sprintf("RBP_raw/%s", tgz_files[i]), exdir = dest_dir)
  setwd(dest_dir)
  dir.create("RBP2")
  subfiles <- list.files()
  rbp_dir <- grep('_intersect_individual_RBP.combined_samples',
                  subfiles, value = TRUE)
  rbp_dirs[i] <- rbp_dir
  if (length(rbp_dir) == 1)
    shell(paste("rename", rbp_dir, "RBP"))
  else
    message(sprintf("Error: %s, %s", i, tgz_files[i]))
  setwd("RBP")
  xzfiles <- list.files()
  rbp_xzs[[i]] <- xzfiles

  for (xzf in xzfiles)
  {
    new_xzf <- gsub(".*_RBP_", "", xzf)
    new_xzf <- gsub(".combined_samples.bed", "", new_xzf, fixed = TRUE)
    shell(paste("rename", xzf, new_xzf))
  }

  setwd(raw_loc)
}

# note: the final xz unzip requires manual work with 7z,
# to the best of my knowledge
setwd(raw_loc)
saveRDS(rbp_dirs, "rbp_dirs.rds")
saveRDS(rbp_xzs, "rbp_xzs.rds")

# now we truly read all the data
setwd(wf_config$raw_loc)
setwd("RBP_S1/a1/RBP2")
rbp_names <- list.files()
n_rbps <- length(rbp_names)
setwd(wf_config$raw_loc)

# data structure: each column is of the form
# 'chrom, start, end, [count per sample]'
# so we take those first 3 rows and turn into colnames
flatten_bed_colnames <- function(chr_start_end)
{
  apply(chr_start_end, 2, function(x) {
    paste(x, collapse = ":")
  })
}

tsv_to_df <- function(file)
{
  data <- read_tsv_text(file)
  # 28x faster than
  # as.data.frame(dplyr::bind_rows(rbp_list))
  setNames(data, sprintf("X%s", seq_along(data))) %>% data.frame()
}

handle_bed_int <- function(bed_int)
{
  new_df <- bed_int[4:nrow(bed_int), ]
  colnames(new_df) <- flatten_bed_colnames(bed_int[1:3, ])
  # yes, you have to eliminate duplicates
  new_df[, !duplicated(colnames(new_df))]
}

remove_zero_cols <- function(df)
{
  df[, !apply(df, 2, function(x) {
    all(x == "0")
  })]
}

# evaluate each RBP into a data.frame
# note: previously took >7 hrs (10 PM to 5:30 AM)
seq_archives <- seq_len(num_archives)

for (rbp_n in seq_len(n_rbps))
{
  rbp_stitch_start <- Sys.time()
  rbp_combined <- empty_named_list(sprintf("X%s", seq_archives))

  rbp_name <- rbp_names[rbp_n]
  cat_f("\n\nStarting %s [i = %s]\n", rbp_name, rbp_n)
  setwd(wf_config$raw_loc)

  for (i in seq_archives)
  {
    rbp_loc <- file.path(sprintf("RBP_S1/a%s/RBP2", i), rbp_name)

    if (file.exists(rbp_loc))
      rbp_combined[[i]] <- tsv_to_df(rbp_loc) %>% handle_bed_int()
    else
      rbp_combined[[i]] <- NA

    cat(".")
  }

  # extract the clean entries of rbp_combined
  rbp_combined <- rbp_combined[sapply(rbp_combined, is.data.frame)]
  cat_f("\nMerging %s pieces of %s: ", length(rbp_combined), rbp_name)

  # takes up too much RAM:
  # rbp_com_tot <- dplyr::bind_rows(rbp_com_clean)
  merge_start_time <- Sys.time()
  rbp_com_tot <- data.table::rbindlist(
    rbp_combined, use.names = TRUE, fill = TRUE) %>% as.data.frame()
  cat_f("done in %.1f seconds\n", time_diff(merge_start_time))

  # we don't expect NAs but check to be sure
  stopifnot(all(!is.na(rbp_com_tot)))

  # remove columns all equal to 0
  rbp_com_df <- remove_zero_cols(rbp_com_tot)

  # for simplicity, here are all the rows w/ dup sample names
  sample_names <- rbp_com_df[["chrom:start:end"]]
  x_dup_rows <- duplicated(sample_names)
  all_dup_rows <- x_dup_rows | duplicated(
    sample_names, fromLast = TRUE)

  # what we don't want: we remove all the duplicate rows (which
  # must be in small_df) and are still left with dup sample names
  small_df <- rbp_com_df[all_dup_rows, ]
  small_unique_df <- small_df[!duplicated(small_df), ]
  stopifnot(is_unique(small_unique_df[["chrom:start:end"]]))

  # now go back to the big df and just remove the dup rows
  rbp_clean_df <- rbp_com_df[!x_dup_rows, ]

  cat_f("%s: (%s, %s) -> (%s, %s) in %.1f seconds\n", rbp_name,
        nrow(rbp_com_tot), ncol(rbp_com_tot),
        nrow(rbp_clean_df), ncol(rbp_clean_df),
        time_diff(rbp_stitch_start))
  saveRDS(rbp_clean_df, sprintf("RBP_S2/com_%s.rds", rbp_name))
}

# 647165 columns total!
nf <- 0
for (rbp_name in rbp_names)
  nf <- nf + ncol(readRDS(sprintf("RBP_S2/com_%s.rds", rbp_name)))
cat_f("columns: %s", nf)

# sum up by sample
rbp_rownames <- empty_named_list(rbp_names)

for (rbp_name in rbp_names)
{
  rbp_com <- readRDS(sprintf("RBP_S2/com_%s.rds", rbp_name))
  # rbp_nums <- convert_to_num(rbp_com[, -1])
  # rownames(rbp_nums) <- NULL
  # rbp_com_file <- sprintf("%s/combined/combined_%s.rds",
  #                         wf_config$pro_loc, rbp_name)
  # saveRDS(rbp_nums, rbp_com_file)

  rbp_rownames[[rbp_name]] <- rbp_com[, 1]
}

rbp_rownames_short <- rbp_rownames$AARS_K562
rbp_rownames_long <- rbp_rownames$ABCF1_K562

# they're pretty much all the same!!!
for (rbp_name in rbp_names)
{
  stopifnot(
    identical(rbp_rownames_short, rbp_rownames[[rbp_name]]) ||
    identical(rbp_rownames_long, rbp_rownames[[rbp_name]])
  )
}

get_self_rds("metadata", wf_config$raw_loc)
app_data <- readRDS("~/shiny-dim-reduction/app/app_data.rds")
for (dep in names(app_data))
  assign_global(dep, app_data[[dep]])

short_dnames <- sprintf("%s.metadata.tsv", rbp_rownames_short)
long_dnames <- sprintf("%s.metadata.tsv", rbp_rownames_long)

short_metadata <- match_metadata_to_samples(
  short_dnames, metadata, "DOWNLOAD_NAME")
long_metadata <- match_metadata_to_samples(
  long_dnames, metadata, "DOWNLOAD_NAME")

order_total$circRNA <- relocate(
  order_total$circRNA, "FASTQ_IDENTIFIER")
order_total$cumulative_ex_genomes <- relocate(
  order_total$cumulative_ex_genomes, "FASTQ_IDENTIFIER")
order_total$rRNA_Species <- relocate(
  order_total$rRNA_Species, "FASTQ_IDENTIFIER")
order_total$Gene_Species <- relocate(
  order_total$Gene_Species, "FASTQ_IDENTIFIER")
order_total$rRNA_Transpose <- relocate(
  order_total$rRNA_Transpose, "species")
order_total$Gene_Transpose <- relocate(
  order_total$Gene_Transpose, "species")

row_axes <- list(
  "miRNA_piRNA_tRNA" = make_axis(
    order_total$miRNA,
    make_row_subsets(order_total$miRNA),
    custom_color_scales
  ),
  "circRNA" = make_axis(
    order_total$circRNA,
    make_row_subsets(order_total$circRNA),
    list()
  ),
  "ex_miRNA" = make_axis(
    order_total$ex_miRNA,
    make_row_subsets(order_total$ex_miRNA),
    custom_color_scales
  ),
  "exogenous" = make_axis(
    order_total$cumulative_ex_genomes,
    make_row_subsets(order_total$cumulative_ex_genomes),
    list()
  ),
  "rRNA_Species" = make_axis(
    order_total$rRNA_Species,
    make_row_subsets(order_total$rRNA_Species),
    list()
  ),
  "Gene_Species" = make_axis(
    order_total$Gene_Species,
    make_row_subsets(order_total$Gene_Species),
    list()
  ),
  "rRNA_Transpose" = make_axis(
    order_total$rRNA_Transpose,
    make_row_subsets(order_total$rRNA_Transpose),
    list()
  ),
  "Gene_Transpose" = make_axis(
    order_total$Gene_Transpose,
    make_row_subsets(order_total$Gene_Transpose),
    list()
  ),
  "rbp_short" = make_axis(
    short_metadata,
    make_row_subsets(short_metadata),
    list()
  ),
  "rbp_long" = make_axis(
    long_metadata,
    make_row_subsets(long_metadata),
    list()
  )
)

col_axes <- list()

for (cat in names(order_total)[1:11])
  col_axes[[cat]] <- make_axis(
    data.frame("SEQ_COUNT" = decorations[[cat]]$COL_SUBSETS$Reference),
    decorations[[cat]]$COL_SUBSETS[-1],
    list()
  )

for (cat in names(order_total)[12:13])
  col_axes[[cat]] <- make_axis(
    data.frame("FASTQ_IDENTIFIER" = decorations[[cat]]$COL_SUBSETS$Reference),
    decorations[[cat]]$COL_SUBSETS[-1],
    list()
  )

rbp_combined_list <- list()
rbp_colnames <- list()

for (rbp_name in rbp_names)
{
  rbp_com_file <- sprintf("%s/combined/combined_%s.rds",
                          wf_config$pro_loc, rbp_name)
  rbp_nums <- readRDS(rbp_com_file)
  rbp_combined_list[[rbp_name]] <- rowSums(rbp_nums)
  rbp_colnames[[rbp_name]] <- colnames(rbp_nums)
}

short_to_long <- function(reads)
{
  x <- numeric(length(rbp_rownames_long))
  x[rbp_rownames_long %in% rbp_rownames_short] <- reads
  x
}

rbp_combined <- data.frame(matrix(nrow = 5346))

for (rbp_name in rbp_names)
{
  if (identical(rbp_rownames[[rbp_name]], rbp_rownames_short))
    rbp_combined[[rbp_name]] <- short_to_long(rbp_combined_list[[rbp_name]])
  else
    rbp_combined[[rbp_name]] <- rbp_combined_list[[rbp_name]]
}

rbp_com <- as.matrix(rbp_combined[, -1])
is_num_data(rbp_com, nrow(rbp_com), ncol(rbp_com))
rbp_com_loc <- sprintf("%s/combined/combined_%s.rds",
                       wf_config$pro_loc, "RNA_binding_proteins")
saveRDS(rbp_com, rbp_com_loc)

# get the unique 150
cols <- colnames(rbp_com)
non_merged <- cols[!grepl("+", cols, fixed = TRUE)]
merged <- cols[grepl("+", cols, fixed = TRUE)]
merged_raw <- gsub("_.*$", "", merged)
test <- setdiff(cols, c(sprintf("%s_K562", merged_raw), sprintf("%s_HepG2", merged_raw)))

col_axes[["RNA_binding_proteins"]] <- make_axis(
  data.frame("RBP" = colnames(rbp_com)),
  list(
    "Unique150" = which(cols %in% test)
  ),
  list()
)

# now add the remaining col_axes
for (rbp_name in rbp_names)
{
  rbp_com_file <- sprintf("%s/combined/combined_%s.rds",
                          wf_config$pro_loc, rbp_name)
  rbp_nums <- readRDS(rbp_com_file)

  rbp_col_sub_temp <- list()

  if (ncol(rbp_nums) > 1000)
  {
    rbp_col_sub_temp[["SD_Top_1000"]] = ind_sd_top(rbp_nums, 1000)
    rbp_col_sub_temp[["SD_Top_100"]] = ind_sd_top(rbp_nums, 100)
  }

  col_axes[[rbp_name]] <- make_axis(
    data.frame("SEQ_COUNT" = colnames(rbp_nums)),
    rbp_col_sub_temp,
    list()
  )
}

are_axes(row_axes)
are_axes(col_axes)

entex_examp2 <- get_self_rds("entex_decorations", wf_config$raw_loc)

app_data$row_axes <- row_axes
app_data$col_axes <- col_axes
app_data$groups <- lapply(categories_full, names)
app_data$categories <- list(
  "miRNA" = make_category("miRNA_piRNA_tRNA", "miRNA"),
  "piRNA" = make_category("miRNA_piRNA_tRNA", "piRNA"),
  "tRNA" = make_category("miRNA_piRNA_tRNA", "tRNA"),
  "circRNA" = make_category("circRNA", "circRNA"),
  "ex_miRNA" = make_category("ex_miRNA", "ex_miRNA"),
  "cumulative_ex_genomes" = make_category("exogenous", "cumulative_ex_genomes"),
  "specific_ex_genomes" = make_category("exogenous", "specific_ex_genomes"),
  "cumulative_ex_ribosomes" = make_category("exogenous", "cumulative_ex_ribosomes"),
  "specific_ex_ribosomes" = make_category("exogenous", "specific_ex_ribosomes"),
  "rRNA_Species" = make_category("rRNA_Species", "rRNA_Species"),
  "Gene_Species" = make_category("Gene_Species", "Gene_Species"),
  "rRNA_Transpose" = make_category("rRNA_Transpose", "rRNA_Transpose"),
  "Gene_Transpose" = make_category("Gene_Transpose", "Gene_Transpose"),
  "RNA_binding_proteins" = make_category("rbp_long", "RNA_binding_proteins")
)

for (rbp_name in rbp_names) {
  n <- length(rbp_rownames[[rbp_name]])

  if (n == app_data$row_axes$rbp_short$length)
    app_data$categories[[rbp_name]] <- make_category("rbp_short", rbp_name)
  else
  {
    stopifnot(n == app_data$row_axes$rbp_long$length)
    app_data$categories[[rbp_name]] <- make_category("rbp_long", rbp_name)
  }
}

categories_match_axes(app_data$categories, app_data$row_axes, app_data$col_axes)
groups_match_categories(app_data$groups, app_data$categories)

rbp_two <- rbp_combined
rbp_two$arch_53 <- NULL
rbp_fin <- dplyr::bind_rows(rbp_two)

rbp_order <- match_metadata_to_samples(
  sprintf("%s.metadata.tsv", lol), metadata, "DOWNLOAD_NAME")
order_total$RNA_binding_proteins <- rbp_order

setwd(dep_loc)
self_save("order_total")

setwd(pro_loc)
saveRDS(as.matrix(rbp_fin), "combined/combined_RNA_binding_proteins.rds")

app_data$groups$`RNA Binding Proteins` <- union(
  app_data$groups$`RNA Binding Proteins`, rbp_names[1:5])

unlist(app_data$groups)

saveRDS(app_data, "~/shiny-dim-reduction/app/app_data.rds")

# ----------
# RAN'S DATA
# ----------

library(data.table)

# quantifications
setwd(raw_loc)
setwd("ran_new")
trna <- data.table::fread("tRNA.csv")
mirna <- data.table::fread("miRNA.csv")
pirna <- data.table::fread("piRNA.csv")
ex_mirna <- data.table::fread("ex_miRNA.csv")

# save combined
setwd(com_loc)
mirna_combined <- ran_df_to_combined(mirna)
saveRDS(mirna_combined, "combined_miRNA.rds")
pirna_combined <- ran_df_to_combined(pirna)
saveRDS(pirna_combined, "combined_piRNA.rds")
trna_combined <- ran_df_to_combined(trna)
saveRDS(trna_combined, "combined_tRNA.rds")
ex_mirna_combined <- ran_df_to_combined(ex_mirna)
saveRDS(ex_mirna_combined, "combined_ex_miRNA.rds")

# fixes some issues with metadata
fix_ran_metadata <- function(metadata)
{
  metadata[metadata == ""] <- "Unknown"

  age10 <- metadata$AGE_10
  age10 <- rep_str(age10, "0 years", "0 to 10")

  for (i in seq_along(age10))
  {
    if (grepl(" y", age10[i]))
    {
      age10[i] <- get_intervals(rep_str(age10[i], " y", ""), 10)
    }
  }

  metadata$AGE_10 <- age10

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

  rownames(metadata) <- NULL
  metadata
}

# open metadata
metadata <- read.csv("meta.csv")
fixed_metadata <- fix_ran_metadata(metadata)

mirna_indices <- match(mirna$FASTQ_IDENTIFIER, metadata$FASTQ_IDENTIFIER)
pirna_indices <- match(pirna$FASTQ_IDENTIFIER, metadata$FASTQ_IDENTIFIER)
trna_indices <- match(trna$FASTQ_IDENTIFIER, metadata$FASTQ_IDENTIFIER)
ex_mirna_indices <- match(ex_mirna$FASTQ_IDENTIFIER, metadata$FASTQ_IDENTIFIER)

# get correctly ordered metadata
mirna_metadata <- fixed_metadata[mirna_indices,]
rownames(mirna_metadata) <- NULL
pirna_metadata <- fixed_metadata[pirna_indices,]
rownames(pirna_metadata) <- NULL
trna_metadata <- fixed_metadata[trna_indices,]
rownames(trna_metadata) <- NULL
ex_mirna_metadata <- fixed_metadata[ex_mirna_indices,]
rownames(ex_mirna_metadata) <- NULL

get_dependency("order_total")
order_total$miRNA <- mirna_metadata
order_total$piRNA <- pirna_metadata
order_total$tRNA <- trna_metadata
order_total$ex_miRNA <- ex_mirna_metadata

# expected outcome
all.equal(order_total$miRNA, order_total$piRNA)
all.equal(order_total$miRNA, order_total$tRNA)

# now update order_total
set_dependency("order_total")

# make categories_full
categories_full <- make_categories_full(exrna_raw_categories_full)
set_dependency("categories_full")

# update decorations
setwd(com_loc)
get_dependency("decorations")
test_dec <- decorations
for (cat in c("miRNA", "piRNA", "tRNA", "ex_miRNA"))
{
  combined <- readRDS(sprintf("combined_%s.rds", cat))
  stopifnot(length(colnames(combined)) == ncol(combined))
  print(dim(combined))

  decorations[[cat]] <- list(
    "CATEGORIES" = cat,
    "ROW_SUBSETS" = list(),
    "COL_SUBSETS"=list("Reference" = colnames(combined))
  )

  for (cha in c("BIOFLUID", "DATASET"))
  {
    if (cha %in% names(order_total[[cat]]))
    {
      for (bio_type in unique(order_total[[cat]][[cha]]))
      {
        indices <- which(order_total[[cat]][[cha]] == bio_type)

        # if there are at least 100 samples with this label
        if (length(indices) >= 100)
          decorations[[cat]][["ROW_SUBSETS"]][[bio_type]] <- indices
      }
    }
  }

  if (ncol(combined) > 1000)
  {
    decorations[[cat]][["COL_SUBSETS"]][["SD_Top_1000"]] = ind_sd_top(combined, 1000)
    decorations[[cat]][["COL_SUBSETS"]][["SD_Top_100"]] = ind_sd_top(combined, 100)
  }
}

for (cat in names(decorations))
{
  print_clean(cat)
  print_clean(isTRUE(all.equal(test_dec[[cat]], decorations[[cat]])))
}

set_dependency("decorations")

# custom color scale
cond_list <- order_total$miRNA$CONDITION[order_total$miRNA$BIOFLUID == "Plasma"]
cond5 <- empty_named_list(unique(cond_list))
for (cond in names(cond5))
{
  cond5[[cond]] <- "#000000"
}

# Healthy
cond5[["Healthy Subject"]] <- "#5D3FD3"
cond5[["Healthy Control"]] <- "#5D3FD3"

# Cognitive: #6088a5
cond5[["Alzheimer's Disease"]] <- "#6088A5"
cond5[["Parkinson's Disease"]] <- "#6088A5"
cond5[["Mild cognitive disorder"]] <- "#6088A5"
cond5[["Dementia associated with Parkinson's Disease"]] <- "#6088A5"
cond5[["Lewy Body Dementia"]] <- "#6088A5"

# Cancer: #cd825a
cond5[["Prostate Carcinoma"]] <- "#CD825A"
cond5[["Pancreatic Carcinoma"]] <- "#CD825A"
cond5[["Colon Carcinoma"]] <- "#CD825A"
cond5[["colorectal cancer"]] <- "#CD825A"

# Subarachnoid hemorrhage: #cd9b5a
cond5[["Subarachnoid Hemorrhage"]] <- "#CD9B5A"

# Type 1 Diabetes Mellites: #e8d9a9
cond5[["Type 1 Diabetes Mellitus"]] <- "#E8D9A9"

# Heart related: #738f57
cond5[["Myocardial Infarction"]] <- "#738F57"
cond5[["Cardiovascular Disorder"]] <- "#738F57"

# Pregnant: #77aab6
cond5[["Pregnant"]] <- "#77AAB6"

custom_color_scales <- list()
custom_color_scales[["CONDITION"]] <- cond5
set_dependency("custom_color_scales")

# add 150 RBPs
# get_dependency("decorations")
# setwd(com_loc)
# rbp_com <- readRDS("combined_RNA_binding_proteins.rds")
#
# decorations$RNA_binding_proteins$COL_SUBSETS$Unique150 <- which(cols %in% test)
# set_dependency("decorations")
#
# # specific RBPs for Robert
# setwd(raw_loc)
# setwd("RBP_S2")
#
# rbp_names <- decorations$RNA_binding_proteins$COL_SUBSETS$Reference
#
# for (rbp_name in rbp_names)
# {
#   hmm <- readRDS(sprintf("com_%s.rds", rbp_name))
#   categories_full[["RNA Binding Proteins"]][[rbp_name]] <- c(nrow(hmm), ncol(hmm) - 1)
# }
#
# set_dependency("categories_full")
#
# good_names <- c("DROSHA_HepG2", "DROSHA_HepG2+K562", "DROSHA_K562",
#                 "DGCR8_HepG2", "DGCR8_HepG2+K562", "DGCR8_K562")
#
# stopifnot(all(good_names %in% rbp_names))
#
# metadata <- order_total$RNA_binding_proteins
#
# hmm <- readRDS(sprintf("com_%s.rds", rbp_name))
# lol <- sprintf("%s.metadata.tsv", hmm[["chrom:start:end"]])
#
# for (rbp_name in good_names)
# {
#   hmm <- readRDS(sprintf("com_%s.rds", rbp_name))
#   lol <- sprintf("%s.metadata.tsv", hmm[["chrom:start:end"]])
#   rbp_order <- match_metadata_to_samples(lol, metadata, "DOWNLOAD_NAME")
#   print(dim(rbp_order))
#   order_total[[rbp_name]] <- rbp_order
# }
#
# set_dependency("order_total")
#
# for (rbp_name in good_names)
# {
#   setwd(raw_loc)
#   setwd("RBP_S2")
#   hmm <- readRDS(sprintf("com_%s.rds", rbp_name))
#   hmm <- hmm[,-1]
#   rownames(hmm) <- NULL
#   hmm_convert <- convert_to_num(hmm)
#   setwd(pro_loc)
#   saveRDS(hmm_convert, sprintf("combined/combined_%s.rds", rbp_name))
# }
#
# for (cat in names(order_total))
# {
#   if (all(names(order_total[[cat]]) == "Unknown"))
#     order_total[[cat]] <- NULL
# }
