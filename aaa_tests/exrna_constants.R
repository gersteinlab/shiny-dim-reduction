# The purpose of this file is to store long texts.

common_cols <- c(
  "DOWNLOAD_NAME", "BIO_NAME", "CONDITION", "BIOFLUID",
  "RNA_SOURCE", "RNA_KIT", "ANATOMICAL", "CELL_SOURCE", "PROFILING",
  "SPECIES", "STANDARDS", "TRANSCRIPTOME", "REFERENCE", "RATIO",
  "BIO_ID", "DATASET", "FASTQ_IDENTIFIER")

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

categories_full <- list(
  "Extracellular RNA"=list(
    "miRNA"=2557,
    "piRNA"=8300,
    "tRNA"=26,
    "circRNA"=743,
    "ex_miRNA"=6792
  ),
  "Exogenous RNA"=list(
    "cumulative_ex_genomes"=3585,
    "specific_ex_genomes"=3022,
    "cumulative_ex_ribosomes"=1127,
    "specific_ex_ribosomes"=1118
  ),
  "Taxonomy"=list(
    "rRNA_Species"=1967,
    "Gene_Species"=1702,
    "rRNA_Transpose"=5260,
    "Gene_Transpose"=5270
  )
)
