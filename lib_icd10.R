
rawdata_folder<-"./data_raw/"
data_folder<-"./data/"

read_icd10_csv<-function() {
  
  folder<-"./data_raw/"
  
  file<-"tblICD10Codes.csv"
  icd10_file<-paste(rawdata_folder,file,sep="")
  icd10 <- read.csv(icd10_file,stringsAsFactors=FALSE)
  
  drops<-c("Status","CategoryID")
  
  icd10<-icd10[,!(names(icd10)%in% drops)]
  
  file<-"tblICD10Ranges.csv"
  icd10_file<-paste(rawdata_folder,file,sep="")
  icd10_ranges <- read.csv(icd10_file,stringsAsFactors=FALSE)
  
  save(icd10,icd10_ranges,file=paste(data_folder,"icd10.RData",sep=""))
}

arrange_icd10_ranges<-function(icd10_ranges,RangeCodeStart,RangeCodeEnd) {
    icd10_ranges<-arrange(icd10_ranges,RangeCodeStart,RangeCodeEnd)
}

arrange_icd10<-function(icd10,icd10_ranges) {
    #vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
    #
    #         Sort icd10 and icd10_ranges by Code
    #
    icd10<-arrange(icd10,Code)
    
    #vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
    #
    #         create icd10$Level[1:3]
    #
    icd10[c("Level1","Level2","Level3")]<-NA
        
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
    
    icd10
}

download_raw_xml<-function(extract = TRUE, delete_zip=FALSE, ...) {
  #######################################
  ##
  ##  set url where files are stored
  ##
  icd10_url<-"http://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/ICD10CM/2015/"
  
  
  #######################################
  ##
  ##  build file url and local file path
  ##
  zipfile<-"ICD10CM_FY2015_Full_XML.zip"
  
  zip_url<-paste(icd10_url,zipfile,sep="")
  dest_file<-paste(rawdata_folder,zipfile,sep="")
  
  #######################################
  ##
  ##  download file url to local file
  ##
  print(paste("Downloading ... ",zipfile,sep=""))
  
  
  res=download.file(url = zip_url,destfile = dest_file,quiet=TRUE)
  
  if(res!=0) return (FALSE)
  ##
  ##  download was successful
  ##
  
  ##############################################
  ##
  ##  extract/unzip local file
  ##
  if (extract) {
    print(paste("Unzipping ... ",zipfile,sep=""))
    exdir<-substring(rawdata_folder,1,nchar(rawdata_folder)-1)
    unzip(dest_file, exdir = exdir,overwrite = T)
  }
  ###############################################
  ##
  ##  delete downloaded (zip) file if indicated
  ##
  if (delete_zip) {
    print(paste("Removing ... ",dest_file,sep=""))
    file.remove(dest_file)
  }
  return (TRUE)
}

extract_icd10_xml<-function() {
  library(XML)
  fileUrl<-paste(rawdata_folder,"FY15_Tabular.xml",sep="")
  
  #  fileUrl <- "http://www.w3schools.com/xml/simple.xml"
  doc <- xmlTreeParse(fileUrl,useInternal=TRUE)
  rootNode <- xmlRoot(doc)
  xmlName(rootNode)
  
  names(rootNode)
  
#   chapterNode<-rootNode[[3]]
#   
#   chapterNode[[3]]
#   chapterNode[[12]][[2]]
#   
#   xmlSApply(rootNode,xmlValue)
  
#   /node Top level node
#   //node Node at any level
#   node[@attr-name] Node with an attribute name
#   node[@attr-name='bob'] Node with attribute name attr-name='bob'

#  chapterNodes<-xpathApply(rootNode,"//chapter",xmlValue)
  chapterNodes<-getNodeSet(doc = doc,path = "//chapter")
  lapply(chapterNodes,function(x) parseChapter(x))
#  xpathSApply(chapterNodes[[1]],"//name",xmlValue)

}

parseChapter<-function(chapterNode){
  chapter<-xpathSApply(chapterNode,"name",xmlValue)
  desc<-xpathSApply(chapterNode,"desc",xmlValue)
  icd10_chapters<<-rbind(icd10_chapters,data.frame(chapter,desc))
  print(paste(chapter,desc,sep="|"))
  sectionNodes<-getNodeSet(chapterNode,path = "section")
  lapply(sectionNodes,function(x,name) parseSection(x,chapter))
}

parseSection<-function(sectionNode,chapter){
  id<-xpathSApply(sectionNode,"@id")
  desc<-xpathSApply(sectionNode,"desc",xmlValue)
  icd10_sections<<-rbind(icd10_sections,data.frame(chapter,id,desc))
  print(paste(".... ",paste(id,desc,sep="|")),sep="")
  diagNodes<-getNodeSet(sectionNode,path = "diag")
  lapply(diagNodes,function(x) parseDiag(x))
  # sectionIndex<-xpathApply(chapterNode,"sectionIndex")
}

parseDiag<-function(diagNode){
  code<-xpathSApply(diagNode,"name",xmlValue)
  desc<-xpathSApply(diagNode,"desc",xmlValue)
  icd10_codes<<-rbind(icd10_codes,data.frame(code,desc))
  #print(paste("........ ",paste(code,desc,sep="|")))
  diagNodes<-getNodeSet(diagNode,path = "diag")
  lapply(diagNodes,function(x) parseDiag(x))
  # sectionIndex<-xpathApply(chapterNode,"sectionIndex")
}
