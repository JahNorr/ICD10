
source("./lib_icd10.R")
source("./lib_icd10_process.R")

yn_download<-FALSE
yn_extract<-FALSE
yn_process<-TRUE
yn_save_processed<- TRUE

if (yn_download) download_raw_xml(extract = T)

if (yn_extract) {
  icd10_chapters<-data.frame(matrix(ncol=2,nrow=0),stringsAsFactors = F)
  colnames(icd10_chapters)<-c("chapter","desc")
  
  icd10_sections<-data.frame(matrix(ncol=3,nrow=0),stringsAsFactors = F)
  colnames(icd10_sections)<-c("chapter","id","desc")
  
  icd10_codes<-data.frame(matrix(ncol=2,nrow=0),row.names = NULL,stringsAsFactors = F)
  colnames(icd10_codes)<-c("code","desc")
  
  x<-extract_icd10_xml()
  save(list = c("icd10_codes","icd10_chapters","icd10_sections"),file="./data/icd10_raw.RData")
}

if (yn_process) {
  icd10_chapters<-process_chapters()
  icd10_sections<-process_sections()
  icd10_codes<-process_codes()
  
  if (yn_save_processed) save(list = c("icd10_codes","icd10_chapters","icd10_sections"),file="./data/icd10.RData")
}

#read_icd10_csv()

#arrange_icd10_ranges(icd10_ranges = icd10_ranges)
