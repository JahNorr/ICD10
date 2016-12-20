library("stringr")


readEquivalenceMappings<-function(reload=F) {
  
  zipname<-"DiagnosisGEMs_2015.zip"
  baseurl<-"https://www.cms.gov/Medicare/Coding/ICD10/Downloads"
  url<-paste(baseurl,zipname,sep="/")
  
  folder<-"./data_raw/equiv_mappings"
  file<-paste(folder,zipname,sep="/")
  #####################################################
  #
  # Download and unzip CMS Equivalence Mapping Files
  #
  if(reload) {
    download.file(url=url,destfile =file)
    unzip(zipfile = file,overwrite = T,exdir = folder)
    file.remove(file)
  }
  
  
  files<-list.files(folder)
  
  ###################################################
  #
  # move pdfs over to documentation
  #
  pdfs<-grep("\\.pdf",files,value=T)
  pdf_folder<-"./documentation/equiv_mappings"
  
  sapply(pdfs,function(x) {
    from<-paste(folder,x,sep="/")
    to<-paste(pdf_folder,x,sep="/")
    file.copy(from =from,to = to )
    file.remove(from)
  })
  
}


getEquivalenceMappings10<-function(){
  
  ##############################################
  #
  # Pull txt files into data frame
  widths=c(7,-1,5,-1,1,1,1,1,1)
  txts<-grep("\\.txt",files,value=T)
  
  txt_9<-paste(folder,grep("I9gem",txts,value=T),sep="/")
  txt_10<-paste(folder,grep("I10gem",txts,value=T),sep="/")
  
  df_10<-read.fwf(file = txt_10,widths = widths,header = F,col.names =  c("source_10","target_9","approximate","no_map","combination","scenario","choice_list"))
  
  df_10$icd10<-str_trim(df_10$icd10)
  df_10$icd9<-str_trim(df_10$icd9)
  
  df_10
}

getEquivalenceMappings9<-function(){
  
  ##############################################
  #
  # Pull txt files into data frame
  widths=c(7,-1,5,-1,1,1,1,1,1)
  txts<-grep("\\.txt",files,value=T)
  
  txt_9<-paste(folder,grep("I9gem",txts,value=T),sep="/")
  txt_9<-paste(folder,grep("I9gem",txts,value=T),sep="/")
  
  df_9<-read.fwf(file = txt_9,widths = widths,header = F,col.names =  c("source_9","target_10","approximate","no_map","combination","scenario","choice_list"))
  
  df_9$icd9<-str_trim(df_9$icd9)
  df_9$icd9<-str_trim(df_9$icd9)
  
  df_9
}



which(df_10$icd9=="25000")



