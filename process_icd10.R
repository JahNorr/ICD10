
source("./lib_icd10.R")

#download_raw_xml(extract = T)

chapters<-data.frame(matrix(ncol=2,nrow=0))
colnames(chapters)<-c("number","desc")

sections<-data.frame(matrix(ncol=3,nrow=0))
colnames(sections)<-c("chapter","id","desc")

diags<-data.frame(matrix(ncol=2,nrow=0))
colnames(diags)<-c("code","desc")

x<-get_xml()


#read_icd10_csv()

#arrange_icd10_ranges(icd10_ranges = icd10_ranges)
