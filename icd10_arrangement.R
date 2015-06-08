#install.packages("plyr")
library(plyr)
#vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
#
#         Sort icd10 and icd10_ranges by Code
#
icd10<-arrange(icd10,Code)
icd10_ranges<-arrange(icd10_ranges,RangeCodeStart,RangeCodeEnd)
 
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

#vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
#
#         create level[1-3] subsets to use for searching for loading
#             into icd10$Level[1:3]
#

icd10_range_level1<-subset(icd10_ranges,
                           subset=Level==1,drop=TRUE)
icd10_range_level1$ICD_Description<-factor(icd10_range_level1$ICD_Description)

icd10_range_level2<-subset(icd10_ranges,
                           subset=Level==2,drop=TRUE)
icd10_range_level2$ICD_Description<-factor(icd10_range_level2$ICD_Description)

 icd10_range_level3<-subset(icd10_ranges,
                            subset=Level==3,drop=TRUE)
icd10_range_level1$ICD_Description<-factor(icd10_range_level1$ICD_Description)
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

#vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
#
#         create icd10$Level[1:3]
#
icd10[c("Level1","Level2","Level3")]<-NA

#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

#vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
#
#         load icd10$Level[1:3]
#
for(n in 1:nrow(icd10)){
  icd10_code<-as.character(icd10[n,"Code"])
  
  icd10_rangeid1<-subset(icd10_range_level1,select=RangeID,
                        subset=as.character(icd10_range_level1$RangeCodeStart)<=icd10_code & 
                          as.character(icd10_range_level1$RangeCodeEnd)>=icd10_code)
  if (nrow(icd10_rangeid1)>0) icd10[n,"Level1"]<-icd10_rangeid1
  
  icd10_rangeid2<-subset(icd10_range_level2,select=RangeID,
                         subset=as.character(icd10_range_level2$RangeCodeStart)<=icd10_code & 
                           as.character(icd10_range_level2$RangeCodeEnd)>=icd10_code)
  if (nrow(icd10_rangeid2)>0) icd10[n,"Level2"]<-icd10_rangeid2
  
  icd10_rangeid3<-subset(icd10_range_level3,select=RangeID,
                         subset=as.character(icd10_range_level3$RangeCodeStart)<=icd10_code & 
                           as.character(icd10_range_level3$RangeCodeEnd)>=icd10_code)
  if (nrow(icd10_rangeid3)>0) icd10[n,"Level3"]<-icd10_rangeid3
}

#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
#
#        Normalize icd10 Code with no decimal pt as Code2
#

icd10$Code2<-lapply(icd10$Code, function(x) paste(substr(x,1,3),substring(x, 5),sep=""))
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

