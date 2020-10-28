


#load in count matrix
proteome_data <- proteome_data_loader("ProteinGroups_Filtered_toMorten.csv")

#load in meta data
meta_data <- meta_data_loader("ProteinGroups_Filtered_toMorten.csv")

protein_key<- protein_key_loader("ProteinGroups_Filtered_toMorten.csv")

res <- data_processor(proteome_data)
rownames<-rownames(proteome_data)
rownames(res)<-rownames

selectedData <- select_sufficient_counts(res, meta_data, 5)