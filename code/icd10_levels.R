# install.packages("dplyr")
# library(dplyr)

icd10_code_levels<-data.frame(row.names = c("code_id","range_id","level"))

icd10$RangeID<-0

for(n in 1:nrow(icd10)){
  icd10_code<-as.character(icd10[n,"Code"])
  icd10_rangeid<-subset(icd10_range_level1,select=RangeID,
                        subset=as.character(icd10_range_level1$RangeCodeStart)<=icd10_code & 
                          as.character(icd10_range_level1$RangeCodeEnd)>=icd10_code)
  if (nrow(icd10_rangeid)>0) icd10[n,"RangeID"]<-icd10_rangeid
}

icd10_new<-merge(icd10,icd10_ranges,by="RangeID")
icd10_new$ICD_Description<-factor(icd10_new$ICD_Description)
# str(icd10_new)
# level1_counts_tmp<-as.data.frame (table(icd10_new$ICD_Description,icd10_new$ICD_Chapter))
# level1_counts<-subset(level1_counts_tmp,subset=Freq>0)
# 
# level1_counts<-level1_counts[order(level1_counts$Freq),]
#   
# par(cex.axis=.75)
# 
# barplot(sort(level1_counts$Freq),names.arg = sort(level1_counts$Var2),ylab = "Count",
#         las=1, col="light gray")
