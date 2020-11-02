


#load in count matrix
proteome_data <- proteome_data_loader("ProteinGroups_Filtered_toMorten.csv")

#load in meta data
meta_data <- meta_data_loader("ProteinGroups_Filtered_toMorten.csv")

protein_key<- protein_key_loader("ProteinGroups_Filtered_toMorten.csv")

res <- data_processor(proteome_data)
rownames<-rownames(proteome_data)
rownames(res)<-rownames

selectedData <- select_sufficient_counts(res, meta_data, 5)
#check our meta_data and selectedData are properly aligned
all(colnames(selectedData) == meta_data$sample)

#MDSPlotGenerator(selectedData, meta_data)


#limma_analysis <- DEG_analysis(selectedData,meta_data,protein_key)

DEG_data <- load_limma_data("limma_results.xlsx")

goResults <- goAnalysis(DEG_data)
#printGOterms(goResults)
NAD_data <- NAD_screener(DEG_data)

