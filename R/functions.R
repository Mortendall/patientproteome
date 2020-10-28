library(fs)
library(here)
library(tidyverse)
library(vroom)
library(data.table)
library(limma)

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

select_sufficient_counts <- function(input_data, meta_data, cutoff) {
  missingSamples <- data.table(is.na(input_data), keep.rownames = TRUE) %>%
    melt(measure.vars = colnames(input_data), variable.name = "sample")

  setnames(missingSamples, "rn", "Accession")
  meta_data <- as.data.table(meta_data)
  meta_data[, sample:=as.character(sample)]

  setkey(meta_data, sample)
  missingSamples <- merge(meta_data, missingSamples, by = "sample")
  missingSamples <- missingSamples[, .(nMissing = sum(value)), by = c("Accession", "Group")] %>%
    dcast(Accession ~ Group, value.var = "nMissing")

  meta_data[, .N, by = "Group"]
  tooManyMissing <- missingSamples[Control > cutoff |
                                     NASH > cutoff , Accession]
  input_data <- input_data[!rownames(input_data) %in% tooManyMissing,]
  input_data <- as.data.frame(input_data)
  return(input_data)
}
