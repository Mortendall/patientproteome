library(fs)
library(here)
library(tidyverse)
library(vroom)
library(data.table)
library(limma)
library(gridExtra)
library(openxlsx)
library(pheatmap)

#' Program to create a count matrix
#'
#' @param file_name name of the csv file containing data
#'
#' @return a count matrix

proteome_data_loader <- function(file_name) {
  data_file <- fs::dir_ls(here("data-raw/"),
             regexp = file_name,
             recurse = TRUE)
  proteome_data <-vroom::vroom(data_file, col_types = cols(
    `[2] 20190726_QE10_nLC0_LiNi_SA_45cm_Atlas_LiverTissue_A2_20190728235150.htrms.PG.Quantity` = col_double(),
    `[5] 20190726_QE10_nLC0_LiNi_SA_45cm_Atlas_LiverTissue_A5_20190730003859.htrms.PG.Quantity` = col_double(),
    `[7] 20190726_QE10_nLC0_LiNi_SA_45cm_Atlas_LiverTissue_A7.htrms.PG.Quantity` = col_double(),
    `[8] 20190726_QE10_nLC0_LiNi_SA_45cm_Atlas_LiverTissue_A8.htrms.PG.Quantity` = col_double(),
    `[9] 20190726_QE10_nLC0_LiNi_SA_45cm_Atlas_LiverTissue_A9_20190730090645.htrms.PG.Quantity` = col_double(),
    `[16] 20190726_QE10_nLC0_LiNi_SA_45cm_Atlas_LiverTissue_B4.htrms.PG.Quantity` = col_double(),
    `[17] 20190726_QE10_nLC0_LiNi_SA_45cm_Atlas_LiverTissue_B5.htrms.PG.Quantity` = col_double(),
    `[20] 20190726_QE10_nLC0_LiNi_SA_45cm_Atlas_LiverTissue_B8.htrms.PG.Quantity` = col_double(),
    `[21] 20190726_QE10_nLC0_LiNi_SA_45cm_Atlas_LiverTissue_B9.htrms.PG.Quantity` = col_double(),
    `[28] 20190726_QE10_nLC0_LiNi_SA_45cm_Atlas_LiverTissue_C4.htrms.PG.Quantity` = col_double(),
    `[30] 20190726_QE10_nLC0_LiNi_SA_45cm_Atlas_LiverTissue_C6.htrms.PG.Quantity` = col_double(),
    `[38] 20190726_QE10_nLC0_LiNi_SA_45cm_Atlas_LiverTissue_D2.htrms.PG.Quantity` = col_double(),
    `[39] 20190726_QE10_nLC0_LiNi_SA_45cm_Atlas_LiverTissue_D3.htrms.PG.Quantity` = col_double(),
    `[41] 20190726_QE10_nLC0_LiNi_SA_45cm_Atlas_LiverTissue_D5.htrms.PG.Quantity` = col_double(),
    `[43] 20190726_QE10_nLC0_LiNi_SA_45cm_Atlas_LiverTissue_D7_20190802141229.htrms.PG.Quantity` = col_double(),
    `[4] 20190726_QE10_nLC0_LiNi_SA_45cm_Atlas_LiverTissue_A4_20190729223222.htrms.PG.Quantity` = col_double(),
    `[10] 20190726_QE10_nLC0_LiNi_SA_45cm_Atlas_LiverTissue_A10.htrms.PG.Quantity` = col_double(),
    `[11] 20190726_QE10_nLC0_LiNi_SA_45cm_Atlas_LiverTissue_A11.htrms.PG.Quantity` = col_double(),
    `[12] 20190726_QE10_nLC0_LiNi_SA_45cm_Atlas_LiverTissue_A12.htrms.PG.Quantity` = col_double(),
    `[13] 20190726_QE10_nLC0_LiNi_SA_45cm_Atlas_LiverTissue_B1.htrms.PG.Quantity` = col_double(),
    `[14] 20190726_QE10_nLC0_LiNi_SA_45cm_Atlas_LiverTissue_B2.htrms.PG.Quantity` = col_double(),
    `[15] 20190726_QE10_nLC0_LiNi_SA_45cm_Atlas_LiverTissue_B3.htrms.PG.Quantity` = col_double(),
    `[22] 20190726_QE10_nLC0_LiNi_SA_45cm_Atlas_LiverTissue_B10.htrms.PG.Quantity` = col_double(),
    `[23] 20190726_QE10_nLC0_LiNi_SA_45cm_Atlas_LiverTissue_B11.htrms.PG.Quantity` = col_double(),
    `[25] 20190726_QE10_nLC0_LiNi_SA_45cm_Atlas_LiverTissue_C1.htrms.PG.Quantity` = col_double(),
    `[26] 20190726_QE10_nLC0_LiNi_SA_45cm_Atlas_LiverTissue_C2.htrms.PG.Quantity` = col_double(),
    `[29] 20190726_QE10_nLC0_LiNi_SA_45cm_Atlas_LiverTissue_C5.htrms.PG.Quantity` = col_double(),
    `[32] 20190726_QE10_nLC0_LiNi_SA_45cm_Atlas_LiverTissue_C8.htrms.PG.Quantity` = col_double(),
    `[33] 20190726_QE10_nLC0_LiNi_SA_45cm_Atlas_LiverTissue_C9.htrms.PG.Quantity` = col_double(),
    `[35] 20190726_QE10_nLC0_LiNi_SA_45cm_Atlas_LiverTissue_C11.htrms.PG.Quantity` = col_double(),
    `[36] 20190726_QE10_nLC0_LiNi_SA_45cm_Atlas_LiverTissue_C12.htrms.PG.Quantity` = col_double(),
    `[40] 20190726_QE10_nLC0_LiNi_SA_45cm_Atlas_LiverTissue_D4.htrms.PG.Quantity` = col_double(),
    `[42] 20190726_QE10_nLC0_LiNi_SA_45cm_Atlas_LiverTissue_D6.htrms.PG.Quantity` = col_double(),
    `[44] 20190726_QE10_nLC0_LiNi_SA_45cm_Atlas_LiverTissue_D8_20190802162100.htrms.PG.Quantity` = col_double(),
    `[45] 20190726_QE10_nLC0_LiNi_SA_45cm_Atlas_LiverTissue_D9.htrms.PG.Quantity` = col_double(),
    PG.ProteinAccessions = col_character(),
    PG.Genes = col_character()
  ))
  proteome_data <- proteome_data[-1, ]
  rownames(proteome_data) <- proteome_data$PG.ProteinAccessions
  count_matrix <- proteome_data[, 1:35]
  rownames(count_matrix) <- rownames(proteome_data)
  return(count_matrix)
}

#' meta data loader
#'
#' @param file_name the location of the meta data file
#'
#' @return

meta_data_loader <- function(file_name){
  data_file <- fs::dir_ls(here("data-raw/"),
                          regexp = file_name,
                          recurse = TRUE)
  proteome_data <- vroom::vroom(data_file)
  meta_data <- as.data.frame(t(proteome_data[1,]))
  meta_data <- meta_data %>%
    mutate(sample = row.names(meta_data))
  colnames(meta_data)[1] = "Group"
  meta_data <- meta_data[1:35,]
  return(meta_data)
}

#' Protein_key_loader
#'
#' @param file_name destination file
#'
#' @return a accession to protein name key

protein_key_loader <- function(file_name) {
  data_file <- fs::dir_ls(here("data-raw/"),
                          regexp = file_name,
                          recurse = TRUE)
  proteome_data <- vroom::vroom(data_file)
  proteome_data <- proteome_data[-1,]
  protein_key <- proteome_data[, 36:37]
  setnames(protein_key,"PG.ProteinAccessions","Accession")
  setnames(protein_key, "PG.Genes","Gene")
  return(protein_key)
}


#' Data processer
#'
#' @param a count matrix with raw proteome data
#'
#' @return a log-transformed, normalzied count matrix

data_processor <- function(proteome_data)
{
  res <- proteome_data
  res[res == "NaN"] <- NA
  res <- log(res)
  res <- normalizeBetweenArrays(res, method = "quantile")
  rownames <- rownames(proteome_data)
  rownames(res)<- rownames
  return(res)
}

#' Select proteins without too many samples missing
#'
#' @param input_data a count matrix
#' @param meta_data your sample groups. ID should be named "Group"
#' @param cutoff cutoff for how many missing samples in a group are too many
#'
#' @return a count matrix where the samples with too few counts are removed

select_sufficient_counts <- function(input_data, meta, cutoff) {
  missingSamples <- data.table(is.na(input_data), keep.rownames = TRUE) %>%
    melt(measure.vars = colnames(input_data), variable.name = "sample")

  setnames(missingSamples, "rn", "Accession")
  meta <- as.data.table(meta)

  setkey(meta, sample)
  missingSamples <- merge(meta, missingSamples, by = "sample")
  missingSamples<- missingSamples %>%
    group_by(Group, Accession) %>%
    summarise(nMissing = sum(value)) %>%
    pivot_wider(names_from = Group, values_from = nMissing)

  cutoff <- 5
  tooManyMissing <- missingSamples %>%
    group_by(Accession) %>%
    filter(Control >cutoff | NASH > cutoff)
  results <- as.data.frame(input_data)
  results <- results %>%
    filter(!rownames(input_data) %in% tooManyMissing$Accession)
 return(results)
}

#' MDS plot generator
#'
#' @param data a count matrix
#' @param meta a setup matrix
#'


MDSPlotGenerator <- function(data, meta){
  mdsData <- plotMDS(data, ndim = 3, plot = FALSE)$cmdscale.out
  mdsData <- cbind(mdsData, meta)
  colnames(mdsData)[1]="V1"
  colnames(mdsData)[2]="V2"
  colnames(mdsData)[3]="V3"
  plot1 <- ggplot(mdsData, aes(x = V1, y = V2, colour = Group)) +
    geom_point() +
    scale_color_brewer(name = "Significant", type = "qual", palette = "Set1")

  plot2 <- ggplot(mdsData, aes(x = V1, y = V3, colour = Group)) +
    geom_point() +
    scale_color_brewer(name = "Significant", type = "qual", palette = "Set1")

  plot3 <- ggplot(mdsData, aes(x = V2, y = V3, colour = Group)) +
    geom_point() +
    scale_color_brewer(name = "Significant", type = "qual", palette = "Set1")

  plots_all <- gridExtra::grid.arrange(plot1, plot2, plot3, ncol = 1)
  dir.create(here("data/figures"), showWarnings = F)
  ggplot2::ggsave(plots_all, filename = here("data/figures/MDSplots.png"),scale = 2.5)

  print("MDSplots have been saved in Data")
}

#' Differentially expressed gene analysis
#'
#' @param data a count matrix
#' @param meta setup data
#' @param proteome_key
#'
#' @return a DEG analysis

DEG_analysis <- function(data, meta,proteome_key){
  design <- stats::model.matrix( ~Group, meta)
  colnames(design) <- c("Control","NASH_vs_con")
  fit <- limma::lmFit(data, design = design, method = "robust")
  fit<- eBayes(fit)
  NASH_vs_con = topTable(fit, coef = "NASH_vs_con", number = Inf, p.value = 1) %>% data.table(keep.rownames = TRUE)

  setnames(NASH_vs_con, "rn","Accession")
  proteome_key<-as.data.table(proteome_key)
  setkey(proteome_key, Accession)
  NASH_vs_con <- merge(NASH_vs_con, proteome_key, by = "Accession") %>%
    arrange(adj.P.Val)
  write.xlsx(NASH_vs_con, file = here("/data/limma_results.xlsx"))
  return(NASH_vs_con)
}


#' Limma result data loader
#'
#' @param file_name name of the limma_results file
#'
#' @return limma analysis

load_limma_data <- function(file_name) {
  data_file <- fs::dir_ls(here("data/"),
                          regexp = file_name,
                          recurse = TRUE)
  limma_analysis <-read.xlsx(data_file)
  return(limma_analysis)
}

#' Gene ontology enrichment analysis of genes generated from a results file
#'
#' @param result_list results table generated from limma. Must be data.table and contain a SYMBOL annotation column
#'
#' @return a list containing enrichresults for each element in the results file list


goAnalysis <- function(result_list){
  result_list <- as.data.table(result_list)
  bg_list <- clusterProfiler::bitr(
    result_list$Gene,
    fromType = "SYMBOL",
    toType = "ENTREZID",
    OrgDb = "org.Hs.eg.db",
    drop = T
  )

    sig_list<- result_list %>%
      dplyr::filter(adj.P.Val<0.05)

    eg <- clusterProfiler::bitr(
      sig_list$Gene,
      fromType = "SYMBOL",
      toType = "ENTREZID",
      OrgDb = "org.Hs.eg.db",
      drop = T
    )
    goResults <- clusterProfiler::enrichGO(gene = eg$ENTREZID,
                                           universe = bg_list$ENTREZID,
                                           OrgDb = org.Hs.eg.db,
                                           ont = "BP")

    return(goResults)
  }

#' File exporter - exports GOresults as an excel sheet, and prints dotplot and cnet plots
#'
#' @param goList a list object containing one or more enrichResults
#'
#' @return

printGOterms <- function(goResult){
  goList <- goResult@result
  openxlsx::write.xlsx(goList, file = here("data/NASH_NAFLD_GOterms.xlsx"), asTable = TRUE)
    dotplot <- enrichplot::dotplot(goResult, title = names(goList))
    ggplot2::ggsave(dotplot, filename = "data/figures/dotplot.png",width = 12, height = 12, units = "cm", scale = 2.5)
    goResult_anno <- clusterProfiler::setReadable(goResult, OrgDb = org.Hs.eg.db, keyType="ENTREZID")
    cnetplot <- enrichplot::cnetplot(goResult_anno, size = 1)
    ggplot2::ggsave(cnetplot, filename = "data/figures/cnetplot.png", scale = 2.5)

    }


#' Title
#'
#' @param limma_results
#'
#' @return
#' @export
#'
#' @examples
NAD_screener <- function(limma_results){
  Selected_candidates <- c("NAMPT",
                           "NMNAT1",
                           "NMNAT2",
                           "NMNAT3",
                           "NMRK1",
                           "NMRK2",
                           "NADSYN1",
                           "NADSYN",
                           "TDO2",
                           "IDO",
                           "NAPRT",
                           "SIRT1",
                           "SIRT3",
                           "PARP1",
                           "CD38",
                           "NNMT",
                           "NADK",
                           "HADH",
                           "KMO",
                           "AFMID")
  NAD_screen <- as.data.table(limma_results) %>%
    filter(limma_results$Gene %in% Selected_candidates)

 volcano_plot <-  ggplot(NAD_screen, aes(x = logFC, y = -log10(adj.P.Val))) +
    geom_point()+
    geom_text(label = NAD_screen$Gene,
              nudge_x = 0.02,
              nudge_y = 0.05,
              check_overlap = T)
 ggsave(volcano_plot, filename = "data/figures/NAD_terms.png", scale = 2.5)
 return(NAD_screen)

}

#' Generates and saves a heatmap of NAD-related terms. Modify for correct regex naming
#'
#' @param count_matrix generated with the proteome data loader
#' @param protein_key containing accession and gene names


heatmap_generator <- function(count_matrix, protein_key){
  Selected_candidates <- c("NAMPT",
                           "NMNAT1",
                           "NMNAT2",
                           "NMNAT3",
                           "NMRK1",
                           "NMRK2",
                           "NADSYN1",
                           "NADSYN",
                           "TDO2",
                           "IDO",
                           "NAPRT",
                           "SIRT1",
                           "SIRT3",
                           "PARP1",
                           "CD38",
                           "NNMT",
                           "NADK",
                           "HADH",
                           "KMO",
                           "AFMID")
  Selected_candidates <- as.data.table(Selected_candidates)
  setnames(Selected_candidates, "Selected_candidates", "Gene")
  Selected_candidates <- merge(Selected_candidates, protein_key, by="Gene")

  Candidates_raw <- count_matrix%>%
    filter(rownames(count_matrix) %in% Selected_candidates$Accession)

  name_key <- rownames(Candidates_raw)
  name_key <-  as.data.table(name_key)
  setnames(name_key, "name_key", "Accession")

  name_key <- merge(name_key, protein_key, by= "Accession")
  name_key %>%
    group_by(Gene) %>%
    count("Gene")

  name_key <- name_key %>%
    mutate(Gene=
             case_when(Accession == "A0A0A0MSE2;E9PF18;Q16836;Q16836-2;Q16836-3" ~ "HADH-1",
                       Accession == "A0A0D9SFP2" ~ "HADH-2",
                       TRUE ~ .$Gene))


  rownames(Candidates_raw)<-name_key$Gene
  colnames(Candidates_raw) <- str_extract(colnames(Candidates_raw), "[0-9][0-9]?")


  heatmap <- pheatmap(Candidates_raw,
                      treeheight_col = 0,
                      treeheight_row = 0,
                      scale = "row",
                      legend = T,
                      na_col = "white",
                      Colv = NA,
                      na.rm = T,
                      cluster_cols = F,
                      fontsize_row = 8,
                      fontsize_col = 8,
                      cellwidth = 8,
                      cellheight = 7
  )
  ggsave(heatmap, filename = here("data/figures/NAD_heatmap.png"), scale = 1.5)

}

#' NAD Metabolic Processes Extractor
#'
#' @param goResults a goResults object
#' @param protein_key an accession to gene name key
#'
#' @return a heatmap saved in the Figures folder

NAD_GO_term_extractor <- function(goResults, protein_key){
  NAD_data <- clusterProfiler::setReadable(goResults, OrgDb = org.Hs.eg.db, keyType="ENTREZID")
  NAD_data <- NAD_data@result
  NAD_terms <- NAD_data %>%
    dplyr::filter(Description == "NAD metabolic process")

  NAD_candidates <- NAD_terms$geneID %>%
    str_split("/", simplify = T)
  NAD_candidates <- t(NAD_candidates)

  Selected_candidates <- as.data.table(NAD_candidates)
  setnames(Selected_candidates, "V1", "Gene")
  Selected_candidates <- merge(Selected_candidates, protein_key, by="Gene")
  Candidates_raw <- proteome_data %>%
    filter(rownames(proteome_data) %in% Selected_candidates$Accession)
  name_key <- rownames(Candidates_raw)
  name_key <-  as.data.table(name_key)
  setnames(name_key, "name_key", "Accession")

  name_key <- merge(name_key, protein_key, by= "Accession")
  name_key %>%
    group_by(Gene) %>%
    count("Gene")
  name_key <- name_key %>%
    mutate(Gene=
             case_when(Accession == "P30613" ~ "PKLR-1",
                       Accession == "P30613-2" ~ "PKLR-2",
                       TRUE ~ .$Gene))
  rownames(Candidates_raw)<-name_key$Gene
  colnames(Candidates_raw) <- str_extract(colnames(Candidates_raw), "[0-9][0-9]?")

  heatmap <- pheatmap(Candidates_raw,
                      treeheight_col = 0,
                      treeheight_row = 0,
                      scale = "row",
                      legend = T,
                      na_col = "white",
                      Colv = NA,
                      na.rm = T,
                      cluster_cols = F,
                      fontsize_row = 8,
                      fontsize_col = 8,
                      cellwidth = 8,
                      cellheight = 7
  )
  ggsave(heatmap, filename = here("data/figures/NAD_heatmap_significant.png"), scale = 1.5)
}
