# Checks first if they are already installed and installs them if they're not
PackagesToInstall<-c('openxlsx',"hablar","ggsci",'BBmisc',"stringr","magick","gridExtra","lubridate","readxl","RColorBrewer","ggplot2","dplyr","rmarkdown","kableExtra","flextable","ggpubr","knitr","scales","tidyr","scales","xml2","rvest","qdapRegex",'cowplot') 
for (i in PackagesToInstall) {
  print(i)
  if (!i %in% installed.packages())
  {install.packages(i)}
  library(i, character.only = TRUE)
}

folder_<-dirname(rstudioapi::getSourceEditorContext()$path)
folderRawData<-paste0(folder_,'/rawdata_to_update')
folderCleanData<-paste0(folder_,'/cleandata_to_update')
folderApp_Single<-paste0(dirname(folder_),'/app_single')
folderApp_Multiple<-paste0(dirname(folder_),'/app_multiple')
#folderAppData<-paste0(dirname(folder_),'/input_to_update')

readSeverityDataset <- function(country) {
  df <- read_excel(paste0(folderRawData,"/SeverityIndex.xlsx"), sheet = country,col_names=FALSE) #DataFrame
  dft <- t(df) #Dataframe has become a character matrix due to transposition
  colnames(dft) <- dft[1,] #Rewriting the column names
  colnames(dft)<-make.names(colnames(dft)) #Make sure column names are in acceptable format, no special character, no space,...
  dft<-dft[-c(1), ] #Removing useless row
  Severity<-dft[,c("X1.4_IND","X4.1_IND","X4.2_IND","X4.3_IND","X4.5_IND","X5_IND","PHSM.SI","Date")] #Selects only fields we are interested in
  Severity<-as.data.frame(Severity) %>% mutate(Date=as.numeric(Date)) %>%
    mutate(Date=as.Date(Date,origin = "1899-12-30")) %>%
    convert(num("X1.4_IND","X4.1_IND","X4.2_IND","X4.3_IND","X4.5_IND","X5_IND","PHSM.SI"))
  #Retransforms the matrix in a dataframe where different types of data are allowed, converts characters into dates or numbers
  Severity$ADM0NAME<-country #Creates new field with country name as the final aim is to have a global dataset with all countries
  Severity<-Severity %>% select(
    Date,
    ADM0NAME,
    GlobalIndex=PHSM.SI,
    Masks= X1.4_IND,
    School = X4.1_IND,
    Workplace=X4.2_IND,
    Gatherings=X4.3_IND,
    StayHome=X4.5_IND,
    Travels=X5_IND,
  ) #Gives understandable column names 
  return(Severity)
}

#Returns list of all countries in the severity excel file
ListCountries_Index<-excel_sheets(paste0(folderRawData,"/SeverityIndex.xlsx"))

#Creates the whole severity dataset with all countries
StringencyIndex<-data.frame()
for (country in ListCountries_Index){
  StringencyIndex_<-readSeverityDataset(country)
  StringencyIndex<-bind_rows(StringencyIndex,StringencyIndex_)
}


write.csv(StringencyIndex,paste0(folderCleanData,'/StringencyIndex.csv'),row.names=FALSE)

ExistingKeyDates_PHSM<-read_excel(paste0(folderCleanData,"/KeyDates.xlsx"),sheet='Severity Index') %>% mutate(Date=as.Date(Date))
ExistingKeyDates_Masks<-read_excel(paste0(folderCleanData,"/KeyDates.xlsx"),sheet='Masks') %>% mutate(Date=as.Date(Date),ADM0NAME=as.character(ADM0NAME))
ExistingKeyDates_Schools<-read_excel(paste0(folderCleanData,"/KeyDates.xlsx"),sheet='Schools') %>% mutate(Date=as.Date(Date),ADM0NAME=as.character(ADM0NAME))
ExistingKeyDates_Businesses<-read_excel(paste0(folderCleanData,"/KeyDates.xlsx"),sheet='Businesses') %>% mutate(Date=as.Date(Date),ADM0NAME=as.character(ADM0NAME))
ExistingKeyDates_Borders<-read_excel(paste0(folderCleanData,"/KeyDates.xlsx"),sheet='Borders') %>% mutate(Date=as.Date(Date),ADM0NAME=as.character(ADM0NAME))
ExistingKeyDates_Movements<-read_excel(paste0(folderCleanData,"/KeyDates.xlsx"),sheet='Movements') %>% mutate(Date=as.Date(Date),ADM0NAME=as.character(ADM0NAME))
ExistingKeyDates_Gatherings<-read_excel(paste0(folderCleanData,"/KeyDates.xlsx"),sheet='Gatherings') %>% mutate(Date=as.Date(Date),ADM0NAME=as.character(ADM0NAME))

StringencyCountryKeyDates<-function(ctr,Measure){
  
  Dataset<-StringencyIndex %>% filter(ADM0NAME==ctr)
  
  if (Measure=='All'){
    Dataset<-Dataset %>% select(Date,ADM0NAME,Index=GlobalIndex)
  }
  
  if (Measure=='Schools'){
    Dataset<-Dataset %>% select(Date,ADM0NAME,Index=School)
  }
  
  if (Measure=='Masks'){
    Dataset<-Dataset %>% select(Date,ADM0NAME,Index=Masks)
  }
  
  if (Measure=='Businesses'){
    Dataset<-Dataset %>% select(Date,ADM0NAME,Index=Workplace)
  }
  
  if (Measure=='Borders'){
    Dataset<-Dataset %>% select(Date,ADM0NAME,Index=Travels)
  }
  
  if (Measure=='Movements'){
    Dataset<-Dataset %>% select(Date,ADM0NAME,Index=StayHome)
  }
  
  if (Measure=='Gatherings'){
    Dataset<-Dataset %>% select(Date,ADM0NAME,Index=Gatherings)
  }
  
  Dataset<-Dataset %>% mutate(Change=if_else(lag(Index,1)==Index,'No','Yes'))

  return(Dataset)
}



Dataset_Schools_<-data.frame()
for (ctr in unique(StringencyIndex$ADM0NAME)){
  Dataset_Schools<-StringencyCountryKeyDates(ctr,'Schools')
  Dataset_Schools_<-rbind(Dataset_Schools,Dataset_Schools_)
}

Dataset_Masks_<-data.frame()
for (ctr in unique(StringencyIndex$ADM0NAME)){
  Dataset_Masks<-StringencyCountryKeyDates(ctr,'Masks')
  Dataset_Masks_<-rbind(Dataset_Masks,Dataset_Masks_)
}

Dataset_Businesses_<-data.frame()
for (ctr in unique(StringencyIndex$ADM0NAME)){
  Dataset_Businesses<-StringencyCountryKeyDates(ctr,'Businesses')
  Dataset_Businesses_<-rbind(Dataset_Businesses,Dataset_Businesses_)
}

Dataset_Gatherings_<-data.frame()
for (ctr in unique(StringencyIndex$ADM0NAME)){
  Dataset_Gatherings<-StringencyCountryKeyDates(ctr,'Gatherings')
  Dataset_Gatherings_<-rbind(Dataset_Gatherings,Dataset_Gatherings_)
}

Dataset_Borders_<-data.frame()
for (ctr in unique(StringencyIndex$ADM0NAME)){
  Dataset_Borders<-StringencyCountryKeyDates(ctr,'Borders')
  Dataset_Borders_<-rbind(Dataset_Borders,Dataset_Borders_)
}

Dataset_All_<-data.frame()
for (ctr in unique(StringencyIndex$ADM0NAME)){
  Dataset_All<-StringencyCountryKeyDates(ctr,'All')
  Dataset_All_<-rbind(Dataset_All,Dataset_All_)
}

Dataset_Movements_<-data.frame()
for (ctr in unique(StringencyIndex$ADM0NAME)){
  Dataset_Movements<-StringencyCountryKeyDates(ctr,'Movements')
  Dataset_Movements_<-rbind(Dataset_Movements,Dataset_Movements_)
}

Dataset_Movements_ <- Dataset_Movements_ %>% mutate(Date=as.Date(Date))
Dataset_All_ <- Dataset_All_ %>% mutate(Date=as.Date(Date))
Dataset_Businesses_ <- Dataset_Businesses_ %>% mutate(Date=as.Date(Date))
Dataset_Borders_ <- Dataset_Borders_ %>% mutate(Date=as.Date(Date))
Dataset_Movements_ <- Dataset_Movements_ %>% mutate(Date=as.Date(Date))
Dataset_Gatherings_ <- Dataset_Gatherings_ %>% mutate(Date=as.Date(Date))
Dataset_Schools_ <- Dataset_Schools_ %>% mutate(Date=as.Date(Date))
Dataset_Masks_ <- Dataset_Masks_ %>% mutate(Date=as.Date(Date))


Dataset_Movements<-Dataset_Movements_ %>% 
  left_join(ExistingKeyDates_Movements %>% select(Date,ADM0NAME,Narrative_Movements),by=c('Date','ADM0NAME')) %>% 
  filter(Change=='Yes') %>% select(-Change)
Dataset_Schools<-Dataset_Schools_ %>% 
  left_join(ExistingKeyDates_Schools %>% select(Date,ADM0NAME,Narrative_Schools),by=c('Date','ADM0NAME')) %>% 
  filter(Change=='Yes') %>% select(-Change)
Dataset_Masks<-Dataset_Masks_ %>% 
  left_join(ExistingKeyDates_Masks %>% select(Date,ADM0NAME,Narrative_Masks),by=c('Date','ADM0NAME')) %>% 
  filter(Change=='Yes') %>% select(-Change)
Dataset_All<-Dataset_All_ %>% 
  left_join(ExistingKeyDates_PHSM %>% select(Date,ADM0NAME,Narrative_All),by=c('Date','ADM0NAME')) %>% 
  filter(Change=='Yes') %>% select(-Change)
Dataset_Borders<-Dataset_Borders_ %>% 
  left_join(ExistingKeyDates_Borders %>% select(Date,ADM0NAME,Narrative_Borders),by=c('Date','ADM0NAME')) %>% 
  filter(Change=='Yes') %>% select(-Change)
Dataset_Businesses<-Dataset_Businesses_ %>% 
  left_join(ExistingKeyDates_Businesses %>% select(Date,ADM0NAME,Narrative_Businesses),by=c('Date','ADM0NAME')) %>% 
  filter(Change=='Yes') %>% select(-Change)
Dataset_Gatherings<-Dataset_Gatherings_ %>% 
  left_join(ExistingKeyDates_Gatherings %>% select(Date,ADM0NAME,Narrative_Gatherings),by=c('Date','ADM0NAME')) %>% 
  filter(Change=='Yes') %>% select(-Change)

list_of_datasets <- list("Severity Index"=Dataset_All,
                         "Masks"=Dataset_Masks,
                         "Schools"=Dataset_Schools,
                         "Businesses"=Dataset_Businesses, 
                         "Gatherings"=Dataset_Gatherings,
                         "Movements"=Dataset_Movements,
                         "Borders"=Dataset_Borders)

write.xlsx(list_of_datasets, file = paste0(folder_,"/KeyDatesUpdated.xlsx"))

