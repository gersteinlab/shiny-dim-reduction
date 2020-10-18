# This script has two goals:
# (i) convert raw data into pure numeric data matrices that satisfy scaling.R
# (ii) use the raw data to process metadata dependencies for the app
# warning: varies highly based on data source.

project_name <- "exRNA"

source("~/Justin-Tool/shiny-dim-reduction/converter.R")

# --------------
# CLEAN ALL DATA
# --------------

# set your directory to a folder with tgz files ... this will untar all
for (file in list.files())
{
  dir.create(repStr(file, ".tgz", ""))
  untar(file, exdir=repStr(file, ".tgz", ""))
}

# then create the folders Cleaned, Raw, Data
dir.create("Cleaned")
dir.create("Raw")
dir.create("Data")

# move all the .tgz files into Raw and all the folders into Data
# Then run this code to make cleaned versions of all folders
base_data_loc <- getwd()
file_list <- list.files(sprintf("%s/Data", base_data_loc))

for (folder in file_list)
{
  subaddr <- sprintf("%s/Data/%s", base_data_loc, folder)
  finaddr <- sprintf("%s/Cleaned/%s", base_data_loc, folder)
  if (!file.exists(finaddr))
    dir.create(finaddr)
  
  memes <- list.files(subaddr)
  splitup <- strsplit(memes, "_exceRpt_", fixed=TRUE)
  actual_names <- unlist(lapply(splitup, function(x){x[2]}))
  processed_names <- gsub("ReadsPerMillion", "RPM", 
                          gsub("TaxonomyTrees", "TAXTREE", 
                               gsub("exogenousGenomes", "EXO-GENO", 
                                    gsub("exogenousRibosomal", "EXO-RIBO",
                                         actual_names))))
  initial_data_locs <- sprintf("%s/%s", subaddr, memes)
  final_data_locs <- sprintf("%s/%s", finaddr, processed_names)
  file.copy(from = initial_data_locs, to = final_data_locs)
}

# -----------------------
# PROCESS SUMMARY REPORTS
# -----------------------

setwd(sprintf("%s/Summary_Reports", raw_loc))

list_of_fragments <- NULL

for (folder in list.files())
{
  for (file in list.files(sprintf("%s/Cleaned", folder)))
  {
    list_of_fragments <- c(list_of_fragments, 
                           sprintf("%s/Cleaned/%s/smallRNAQuants_RPM.RData", 
                                   folder, file))
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
gencode <- miRNA

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
  gencode[[i]] <- exprs.gencode.rpm %>% raw_to_df()
}

miRNA <- dplyr::bind_rows(miRNA)
piRNA <- dplyr::bind_rows(piRNA)
tRNA <- dplyr::bind_rows(tRNA)
circRNA <- dplyr::bind_rows(circRNA)
ex_miRNA <- dplyr::bind_rows(ex_miRNA)
cumulative_ex_genomes <- dplyr::bind_rows(cumulative_ex_genomes)
specific_ex_genomes <- dplyr::bind_rows(specific_ex_genomes)
cumulative_ex_ribosomes <- dplyr::bind_rows(cumulative_ex_ribosomes)
specific_ex_ribosomes <- dplyr::bind_rows(specific_ex_ribosomes)
gencode <- dplyr::bind_rows(gencode)

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
gencode <- rem_dup(gencode)

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
saveRDS(gencode, "gencode.rds")

# ---------------------
# METADATA AND TAXONOMY
# ---------------------

parse_ref <- function(list_of)
{
  lengths <- lapply(list_of, length)
  shortened <- list_of[lengths > 1]
  
  if (shortened[[1]][1] == "#DOWNLOAD NAME")
    shortened[[1]][1] <- "DOWNLOAD NAME"
  
  do.call(rbind, shortened) %>% r1_to_cols() %>% data.frame()
}

bios_list <- my_empty_list(sprintf("M%s", 1:3))
expe_list <- bios_list
dono_list <- bios_list
rrna_list <- bios_list
gene_list <- bios_list

for (i in 1:3)
{
  setwd(sprintf("%s/Metadata_Taxonomy/Ref_New", raw_loc))
  bios_list[[i]] <- read_tsv_text(sprintf("Bios_%s.tsv", i)) %>% parse_ref()
  expe_list[[i]] <- read_tsv_text(sprintf("Expe_%s.tsv", i)) %>% parse_ref()
  dono_list[[i]] <- read_tsv_text(sprintf("Dono_%s.tsv", i)) %>% parse_ref()
  rrna_list[[i]] <- read_tsv_text(sprintf("rRNA_%s.tsv", i)) %>% parse_ref()
  gene_list[[i]] <- read_tsv_text(sprintf("Gene_%s.tsv", i)) %>% parse_ref()
}

order_by_col <- function(data, column)
{
  data[order(data[,column]),]
}

bind_fancy <- function(my_list)
{
  ref <- dplyr::bind_rows(my_list)
  ref[!duplicated(ref), ] %>% order_by_col("FASTQ.IDENTIFIER")
}

bios_ref <- bind_fancy(bios_list)
expe_ref <- bind_fancy(expe_list)
dono_ref <- bind_fancy(dono_list)
rrna_ref <- bind_fancy(rrna_list)
gene_ref <- bind_fancy(gene_list)

common <- dono_ref[,3:18]
colnames(common) <- c(
  "BIO_NAME", "CONDITION", "BIOFLUID", "RNA_SOURCE", "RNA_KIT", 
  "ANATOMICAL", "CELL_SOURCE", "PROFILING", "SPECIES", "STANDARDS", 
  "REFERENCE", "TRANSCRIPTOME", "RATIO", "BIO_ID", "DATASET", "FASTQ_IDENTIFIER")

stitched <- list("BIOS" = bios_ref[,1:2], 
                 "EXPE" = expe_ref[,1:2], 
                 "DONO" = dono_ref[,1:2], 
                 "RRNA" = rrna_ref[,1:2],
                 "GENE" = gene_ref[,1:2])

dest_folders <- sprintf("Metadata_Taxonomy/%s_Mass", 
                        c("Bios", "Expe", "Dono", "rRNA", "Gene"))

rm(bios_ref,bios_list,
   expe_ref,expe_list,
   dono_ref,dono_list,
   rrna_ref,rrna_list,
   gene_ref,gene_list)

# --------------------
# DOWNLOAD AND CONVERT
# --------------------

setwd(raw_loc)

# download a ton of URLs
for (i in 1:5)
{
  dir.create(dest_folders[i])
  file_name_list <- stitched[[i]][[1]]
  url_list <- stitched[[i]][[2]]
  file_parts <- strsplit(file_name_list[1], ".", fixed=TRUE)[[1]]
  file_extension <- rev(unlist(file_parts))[1]
  final_len <- length(url_list)
  sequence <- c(seq(1, final_len, 40), final_len+1)  
  dest_list <- sprintf("%s/%s/M%s.%s", raw_loc, 
                       dest_folders[i], 1:final_len, file_extension)
  
  # some parts might be missing after this ... gonna have to redo them
  for (j in 1:(length(sequence)-1))
    download.file(url_list[sequence[j]:(sequence[j+1]-1)], 
                  dest_list[sequence[j]:(sequence[j+1]-1)], 
                  method="libcurl", quiet=FALSE)
}

# catch the missing ones
for (i in 1:5)
{
  file_name_list <- stitched[[i]][[1]]
  url_list <- stitched[[i]][[2]]
  file_parts <- strsplit(file_name_list[1], ".", fixed=TRUE)[[1]]
  file_extension <- rev(unlist(file_parts))[1]
  final_len <- length(url_list)
  sequence <- c(seq(1, final_len, 40), final_len+1)  
  dest_list <- sprintf("%s/%s/M%s.%s", raw_loc, 
                       dest_folders[i], 1:final_len, file_extension)
  
  for (j in 1:length(dest_list))
  {
    if (!file.exists(dest_list[j]))
    {
      tryCatch(
        download.file(url_list[j], dest_list[j], quiet=TRUE), 
        warning = function(e){
          print(sprintf("W: %s, %s", i, j))
        },
        error = function(e){
          print(sprintf("E: %s, %s", i, j))
        },
        finally=NULL
      )
    }
  }
}

# ----------------
# BIOS, DONO, EXPE
# ----------------
bed_txt <- my_empty_list(c("Bios", "Expe", "Dono"))

for (i in 1:3)
{
  print(sprintf("i: %s", i))
  
  file_name_list <- stitched[[i]][[1]]
  url_list <- stitched[[i]][[2]]
  file_parts <- strsplit(file_name_list[1], ".", fixed=TRUE)[[1]]
  file_extension <- rev(unlist(file_parts))[1]
  final_len <- length(url_list)
  bed_txt[[i]] <- my_empty_list(sprintf("M%s.%s", 1:final_len, file_extension))
  dest_list <- sprintf("%s/%s/M%s.%s", raw_loc, 
                       dest_folders[i], 1:final_len, file_extension)
  
  for (j in 1:final_len)
  {
    if (j %% 1000 == 0)
      print(sprintf("j: %s", j))
    bed_txt[[i]][[j]] <- do.call(rbind, read_tsv_text(dest_list[j]))
  }
}

empty_df <- matrix(0, nrow=1, ncol=1) %>% data.frame()
colnames(empty_df) <- "Unknown"

bed_total <- my_empty_list(c("Bios", "Expe", "Dono"))
for (i in 1:3)
{
  hmm <- lapply(bed_txt[[i]], function(x){
    if (is.null(x))
      return(empty_df)
    x[,1] <- make.unique(x[,1])
    t(x) %>% r1_to_cols() %>% data.frame()
  })
  
  bed_total[[i]] <- dplyr::bind_rows(hmm)
}

setwd(raw_loc)
saveRDS(bed_total, "Metadata_Taxonomy/bed_total.rds")

setwd(raw_loc)
bed_total <- readRDS("Metadata_Taxonomy/bed_total.rds")

bios_total <- bed_total[[1]]
expe_total <- bed_total[[2]]
dono_total <- bed_total[[3]]

frac_acceptable <- function(derp){
  x1 <- length(which(is.na(derp)))
  x2 <- length(which(is.null(derp)))
  x3 <- length(which(is.nan(derp)))
  x4 <- length(which(derp == ""))
  x5 <- length(which(derp == "Unknown"))
  1-(x1+x2+x3+x4+x5)/length(derp)
}

check_garbo <- function(data, min) {
  select_if(data, function(x){frac_acceptable(x) > min})
}

indices <- which(common$PROFILING != "qPCR")
bios_c <- check_garbo(bios_total[indices,], 0.3)
expe_c <- check_garbo(expe_total[indices,], 0.3)
dono_c <- check_garbo(dono_total[indices,], 0.5)

# dono
make_age_range <- function(x, scale){
  num <- floor(as.numeric(x)/scale)*scale
  sprintf("%s to %s", num, num+scale)
}

dono_clean <- dono_c
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
expe_clean$X.property <- NULL
expe_clean$X..Status <- NULL
expe_clean$X..exRNA.Source.Isolation.Protocol <- NULL
expe_clean$X...Protocol.Description <- NULL
expe_clean$X..exRNA.Sample.Preparation.Protocol <- NULL
expe_clean$X..Schema.Version <- NULL
expe_clean$X...Biofluid <- NULL
expe_clean$X....Description <- NULL
expe_clean$X...Protocol.Description.1 <- NULL
expe_clean$X...smRNA.Seq <- NULL
expe_clean$X..Experiment.Type <- NULL
expe_clean$X....Other.exRNA.Quantification.Method <- NULL
expe_clean$X......Low.Speed.Centrifugation <- NULL
expe_clean$X....Library.Generation <- NULL
expe_clean$X......Other.Library.Construction.Kit <- NULL
expe_clean$X.....Other.Kits <- NULL
expe_clean$X...RNA.Isolation.Method <- NULL
# colnames(expe_clean) <- c(
#   "EXPERIMENT", "EXPERIMENT_TYPE", "AMPLIFIED", 
#   "DNA_QUANTIFICATION_METHOD", "LIBRARY_CONSTRUCTION_KIT", "SAMPLES_MULTIPLEXED", 
#   "STRAND_SPECIFICITY", "STARTING_MATERIAL_TYPE", "EXRNA_QUANTIFICATION_METHOD", 
#   "RNA_ISOLATION_KIT", "EXTRACELLULAR_VESICLE_PURIFICATION", "CELL_REMOVAL", 
#   "CELL_REMOVAL_METHOD", "CENTRIFUGATION_PARAMETERS"
# )
colnames(expe_clean) <- c(
  "EXPERIMENT", "CELL_REMOVAL", "ENZYMATIC_TREATMENT",
  "EXRNA_QUANTIFICATION_METHOD","RNA_ISOLATION_KIT", "PROTEINASE_K", "DNASE",
  "EXTRACELLULAR_VESICLE_PURIFICATION", "CELL_REMOVAL_METHOD",
  "CENTRIFUGATION_PARAMETERS","AMPLIFIED", "DNA_QUANTIFICATION_METHOD", 
  "LIBRARY_CONSTRUCTION_KIT",  "SAMPLES_MULTIPLEXED", 
  "STRAND_SPECIFICITY", "STARTING_MATERIAL_TYPE")

# note: redundancy between columns and missing data have been fixed
# keep all factors, even if length(unique) < 2 or length(unique) > num_filters
metadata <- cbind(common[indices,], bios_clean, expe_clean, dono_clean)

metadata$BIOSAMPLE <- NULL
metadata$RNA_ISOLATION_KIT <- NULL

metadata$CONDITION <- repStr(
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

metadata$BIOFLUID <- repStr(
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

metadata$ANATOMICAL <- repStr(
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

metadata$RNA_KIT <- repStr(
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

metadata$RNA_SOURCE <- repStr(
  metadata$RNA_SOURCE,
  "HDL-containing protein-lipid-RNA complex",
  "HDL-protein-lipid-RNA complex"
)

metadata$BIO_ID <- repStr(
  metadata$BIO_ID,
  c("EXR-", "-BS"),
  c("", "")
)

metadata$EXPERIMENT <- repStr(
  metadata$EXPERIMENT,
  c("EXR-", "-EX"),
  c("", "")
)

metadata$DATASET <- repStr(
  metadata$DATASET,
  c("EXR-", "-AN"),
  c("", "")
)

metadata$DONOR <- repStr(
  metadata$DONOR,
  c("EXR-", "-DO"),
  c("", "")
)

metadata$LIBRARY_CONSTRUCTION_KIT <- repStr(
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

metadata$SEX <- repStr(
  metadata$SEX,
  c("Gender unknown", "Gender unspecified", "female", "male", 
    "Masculine gender", "FeMale"),
  c("Unknown", "Unknown", "Female", "Male", "Male", "Female")
)

metadata$CENTRIFUGATION_PARAMETERS <- repStr(
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
saveRDS(metadata, "Metadata_Taxonomy/metadata.rds")

# ---------------------
# TAXONOMY (Gene, rRNA)
# ---------------------
rg_txt <- my_empty_list(c("rRNA", "Gene"))

for (i in 4:5)
{
  print(sprintf("i: %s", i))
  
  file_name_list <- stitched[[i]][[1]]
  url_list <- stitched[[i]][[2]]
  file_parts <- strsplit(file_name_list[1], ".", fixed=TRUE)[[1]]
  file_extension <- rev(unlist(file_parts))[1]
  final_len <- length(url_list)
  rg_txt[[i-3]] <- my_empty_list(sprintf("M%s.%s", 1:final_len, file_extension))
  dest_list <- sprintf("%s/%s/M%s.%s", raw_loc, 
                       dest_folders[i], 1:final_len, file_extension)
  
  for (j in 1:final_len)
  {
    if (j %% 1000 == 0)
      print(sprintf("j: %s", j))
    rg_txt[[i-3]][[j]] <- do.call(rbind, read_tsv_text(dest_list[j]))
  }
}
for (i in 1:2)
{
  for (j in 1:length(rg_txt[[i]]))
  {
    rg_txt[[i]][[j]] <- rg_txt[[i]][[j]] %>% r1_to_cols() %>% data.frame()
  }
}

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

# a super-optimized function for populating the tree to species only
species_only <- function(hmm1)
{
  working <- rep(0, 24)
  
  for (i in 1:nrow(hmm1))
  {
    if (hmm1[i, 25] != 0)
    {
      hmm1[i, 1:24] <- working
    }
    else
    {
      for (j in 24:1)
      {
        if (hmm1[i,j] != 0)
        {
          working[j] <- hmm1[i,j]
          break
        }
        else
        {
          working[j] <- 0
        }
      }
    }
  }
  
  hmm1[hmm1[,25] != 0,]
}

rg_assoc <- my_empty_list(names(rg_txt))
rg_txt2 <- rg_assoc

empty_df <- matrix(0, nrow=1, ncol=1) %>% data.frame()
colnames(empty_df) <- "Unknown"
start <- my_timer()

for (n in 1:2)
{
  print(n)
  rg_assoc[[n]] <- my_empty_list(names(rg_txt[[n]]))
  rg_txt2[[n]] <- rg_assoc[[n]]
  for (k in 1:length(rg_txt[[n]]))
  {
    if (k %% 30 == 0)
    {
      print(sprintf("k: %s", k))
      print(sprintf("Average: %s", my_timer(start)/k))
    }
      
    test <- rg_txt[[n]][[k]]
    
    if (length(test) < 1 || nrow(test) < 1 || ncol(test) != 7)
    {
      rg_assoc[[n]][[k]] <- empty_df
      rg_txt2[[n]][[k]] <- empty_df
    }
    else
    {
      x <- test[test$level != "no rank",c(2,3,5,6)]
      
      if (length(which(x$level == "species")) < 1 || ncol(x) != 4)
      {
        rg_assoc[[n]][[k]] <- empty_df
        rg_txt2[[n]][[k]] <- empty_df
      }
      else
      {
        hmm1 <- matrix(0, nrow=nrow(x), ncol=length(taxonomic_ordering)) %>% data.frame()
        for (i in 1:nrow(x))
          hmm1[i, taxonomic_ordering == x[i, 1]] <- x[i, 3]
        
        hmm2 <- species_only(hmm1)[,1:25]
        readCounts <- x[as.numeric(rownames(hmm2)),,drop=FALSE]
        
        rownames(readCounts) <- apply(readCounts, 1, 
                                      function(a){sprintf("%s_%s", a[3], a[2])})
        rownames(hmm2) <- rownames(readCounts)
        indices <- readCounts[,4] > 0
        
        rg_assoc[[n]][[k]] <- hmm2[indices,]
        rg_txt2[[n]][[k]] <- t(readCounts[indices,4,drop=FALSE]) %>% data.frame()
      }
    }
  }
}

# now that all the cleaning is done, save ...
setwd(raw_loc)
saveRDS(rg_txt2, "Metadata_Taxonomy/rg_txt2.rds")
saveRDS(rg_assoc, "Metadata_Taxonomy/rg_assoc.rds")

setwd(raw_loc)
rg_txt2 <- readRDS("Metadata_Taxonomy/rg_txt2.rds")
rg_assoc <- readRDS("Metadata_Taxonomy/rg_assoc.rds")
good_indices <- my_empty_list(names(rg_txt2))

# find and keep only actual samples
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

# quickly binds rows
iterative_bind_rows <- function(data)
{
  len <- length(data)
  
  if (len < 512)
  {
    print(len)
    return(dplyr::bind_rows(data))
  }
  
  midpoint <- floor(len/2)
  d1 <- data[1:midpoint]
  d2 <- data[(midpoint+1):len]
  
  dplyr::bind_rows(iterative_bind_rows(d1), iterative_bind_rows(d2))
}

# bind them all together!
all_rRNA <- iterative_bind_rows(rg_txt2[[1]])
rownames(all_rRNA) <- good_indices[[1]]
assoc_rRNA <- iterative_bind_rows(rg_assoc[[1]])
assoc_rRNA <- assoc_rRNA[!duplicated(assoc_rRNA$X25),]
all_gene <- iterative_bind_rows(rg_txt2[[2]])
rownames(all_gene) <- good_indices[[2]]
assoc_gene <- iterative_bind_rows(rg_assoc[[2]])
assoc_gene <- assoc_gene[!duplicated(assoc_gene$X25),]

# saving
setwd(raw_loc)
saveRDS(all_rRNA, "Metadata_Taxonomy/all_rRNA.rds")
saveRDS(assoc_rRNA, "Metadata_Taxonomy/assoc_rRNA.rds")
saveRDS(all_gene, "Metadata_Taxonomy/all_gene.rds")
saveRDS(assoc_gene, "Metadata_Taxonomy/assoc_gene.rds")

setwd(raw_loc)
all_rRNA <- readRDS("Metadata_Taxonomy/all_rRNA.rds")
assoc_rRNA <- readRDS("Metadata_Taxonomy/assoc_rRNA.rds")
all_gene <- readRDS("Metadata_Taxonomy/all_gene.rds")
assoc_gene <- readRDS("Metadata_Taxonomy/assoc_gene.rds")

# more cleaning
cleanup_fun_2 <- function(data, frac){
  data[,colSums(is.na(data)) < frac*nrow(data), drop=FALSE]
}

all_rRNA_clean <- cleanup_fun_2(all_rRNA, 0.99)
all_gene_clean <- cleanup_fun_2(all_gene, 0.99)
assoc_rRNA_clean <- assoc_rRNA[colnames(all_rRNA) %in% colnames(all_rRNA_clean),]
assoc_gene_clean <- assoc_gene[colnames(all_gene) %in% colnames(all_gene_clean),]

setwd(raw_loc)
saveRDS(all_rRNA_clean, "Metadata_Taxonomy/all_rrna_clean.rds")
saveRDS(assoc_rRNA_clean, "Metadata_Taxonomy/assoc_rrna_clean.rds")
saveRDS(all_gene_clean, "Metadata_Taxonomy/all_gene_clean.rds")
saveRDS(assoc_gene_clean, "Metadata_Taxonomy/assoc_gene_clean.rds")

# ---------
# FINISHING
# ---------

setwd(raw_loc)
metadata <- readRDS("Metadata_Taxonomy/metadata.rds")

setwd(raw_loc)
all_rRNA_clean <- readRDS("Metadata_Taxonomy/all_rrna_clean.rds")
assoc_rRNA_clean <- readRDS("Metadata_Taxonomy/assoc_rrna_clean.rds")
rownames(assoc_rRNA_clean) <- NULL
colnames(assoc_rRNA_clean) <- taxonomic_ordering[1:25]
all_gene_clean <- readRDS("Metadata_Taxonomy/all_gene_clean.rds")
assoc_gene_clean <- readRDS("Metadata_Taxonomy/assoc_gene_clean.rds")
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

cut_features <- function(data, thresh){
  bad_indices <- NULL
  
  for (j in 2:ncol(data))
  {
    if (sum(is.na(data[[j]])) > thresh*nrow(data))
      bad_indices <- c(bad_indices, j)
  }
  
  good_indices <- setdiff(1:ncol(data), bad_indices)
  data[,good_indices]
}

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

assoc_rRNA_clean[assoc_rRNA_clean == 0] <- "Unknown"
assoc_gene_clean[assoc_gene_clean == 0] <- "Unknown"

order_total <- list("miRNA"=match_order(miRNA[,1], metadata), 
                    "piRNA"=match_order(piRNA[,1], metadata), 
                    "tRNA"=match_order(tRNA[,1], metadata), 
                    "circRNA"=match_order(circRNA[,1], metadata), 
                    "ex_miRNA"=match_order(ex_miRNA[,1], metadata),
                    "cumulative_ex_genomes"=
                      match_order(cumulative_ex_genomes[,1], metadata),
                    "specific_ex_genomes"=
                      match_order(specific_ex_genomes[,1], metadata),
                    "cumulative_ex_ribosomes"=
                      match_order(cumulative_ex_ribosomes[,1], metadata),
                    "specific_ex_ribosomes"=
                      match_order(specific_ex_ribosomes[,1], metadata),
                    "rRNA_Species"=metadata[rownames(all_rRNA_clean),],
                    "Gene_Species"=metadata[rownames(all_gene_clean),],
                    "rRNA_Transpose"=assoc_rRNA_clean,
                    "Gene_Transpose"=assoc_gene_clean)

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
rRNA_species <- all_rRNA_clean
gene_species <- all_gene_clean
rRNA_transpose <- t(all_rRNA_clean)
gene_transpose <- t(all_gene_clean)
rownames(rRNA_species) <- NULL
rownames(gene_species) <- NULL
rownames(rRNA_transpose) <- NULL
rownames(gene_transpose) <- NULL
colnames(rRNA_transpose) <- order_total$rRNA_Samples$FASTQ_IDENTIFIER
colnames(gene_transpose) <- order_total$Gene_Samples$FASTQ_IDENTIFIER

categories <- list(
  "Extracellular RNA"=list(
    "miRNA"=ncol(miRNA),
    "piRNA"=ncol(piRNA),
    "tRNA"=ncol(tRNA),
    "circRNA"=ncol(circRNA),
    "ex_miRNA"=ncol(ex_miRNA)
  ),
  "Exogenous RNA"=list(
    "cumulative_ex_genomes"=5431,
    "specific_ex_genomes"=5431,
    "cumulative_ex_ribosomes"=1317,
    "specific_ex_ribosomes"=1317
  ),
  "Taxonomy"=list(
    "rRNA_Species"=1967,
    "Gene_Species"=1702,
    "rRNA_Transpose"=5260,
    "Gene_Transpose"=5276
  )
)

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

# -----------------------
# LIBRARIES AND FUNCTIONS
# -----------------------

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

setwd(dep_loc)
saveRDS(categories, "categories_full.rds")
saveRDS(order_total, "order_total.rds")

# amazon_keys <- c("AKIAVI2HZGPODUFE62HE",
#                  "V4LyDo0i1zv2cUZaFeIg9EFUFe+Fr+cv05U30efG",
#                  "shiny-app-data-justin-2")
amazon_keys <- c("AKIAVI2HZGPODUFE62HE",
                 "V4LyDo0i1zv2cUZaFeIg9EFUFe+Fr+cv05U30efG",
                 "shiny-app-data-justin-exrna")
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
perplexity_types <- c(10, 20, 30, 50, 100)
pc_cap <- 10
user_credentials <- list("guest"=hashpw("All@2019", gensalt(12)))

myRDS("amazon_keys.rds", amazon_keys)
myRDS("app_title.rds", app_title)
myRDS("app_citations.rds", app_citations)
myRDS("perplexity_types.rds", perplexity_types)
myRDS("pc_cap.rds", pc_cap)
myRDS("user_credentials.rds", user_credentials)