

read_icd10_csv<-function() {
  
  folder<-"./data_raw/"
  
  file<-"tblICD10Codes.csv"
  icd10_file<-paste(folder,file,sep="")
  icd10 <- read.csv(icd10_file,stringsAsFactors=FALSE)
  
  drops<-c("Status","CategoryID")
  
  icd10<-icd10[,!(names(icd10)%in% drops)]
  
  file<-"tblICD10Ranges.csv"
  icd10_file<-paste(folder,file,sep="")
  icd10_ranges <- read.csv(icd10_file,stringsAsFactors=FALSE)
  
  save(icd10,icd10_ranges,file="./data/icd10.RData")
}