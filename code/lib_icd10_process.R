
process_chapters<-function() {
  
  # get rid of factors, if present
  #
  icd10_chapters$chapter<-as.integer(icd10_chapters$chapter)
  icd10_chapters$desc<-as.character(icd10_chapters$desc)
  
  # get local vars to work with
  #
  desc=icd10_chapters$desc
  
  # find the start end end positions 
  # for the surrounding parens for the range
  #
  parens<-gregexpr("[()]",desc,)# ,fixed=T)
  start<-sapply(parens,function(x) x[1])
  end<-sapply(parens,function(x) x[2])
  
  # set the desc to be the original without the range
  #
  icd10_chapters$desc<-as.character(mapply(function(x,y) substring(x,first = 1,last = y-1),desc,start))
  
  # cut out the text containing the range
  #
  ranges<-as.character(mapply(function(x,y,z) substring(x,first = y+1,last = z-1),desc,start,end))
  
  # create columns for the start and end ranges
  # for the chapter
  #
  icd10_chapters$range_start<-substring(ranges,1,3)
  icd10_chapters$range_end<-substring(ranges,5,7)
  icd10_chapters
}


process_sections<-function() {
  
  rownames(icd10_sections)<-NULL
  
  # get rid of factors, if present
  #
  icd10_sections$chapter<-as.integer(icd10_sections$chapter)
  icd10_sections$desc<-as.character(icd10_sections$desc)
  icd10_sections$id<-as.character(icd10_sections$id)
  
  # get local vars to work with
  #
  desc=icd10_sections$desc
  ids=icd10_sections$id
  
  # find the start end end positions 
  # for the surrounding parens for the range
  #
  parens<-gregexpr("[(]",desc,)# ,fixed=T)
  start<-sapply(parens,function(x) x[1])
  #  end<-sapply(parens,function(x) x[2])
  
  # set the desc to be the original without the range
  #
  icd10_sections$desc<-as.character(mapply(function(x,y) substring(x,first = 1,last = y-1),desc,start))
  
  # create columns for the start and end ranges
  # for the section
  #
  
  icd10_sections$range_start<-substring(ids,1,3)
  icd10_sections$range_end<-as.character(lapply(ids,function(x) if (nchar(x)>3) substring(x,5,7) else substring(x,1,3)))
  
  icd10_sections<-icd10_sections[,!(names(icd10_sections) %in% c("id"))]
  icd10_sections$section<-c(1:nrow(icd10_sections))
  icd10_sections<-cbind(icd10_sections[,"section"],
                        icd10_sections[,!(names(icd10_sections) %in% c("section"))])
  colnames(icd10_sections)[1]<-"section"
  
  levels<-as.integer(mapply(function(x,y) {
    code<-x
    length(grep(pattern = code,x =icd10_sections$range_start[icd10_sections$section>=y] ))
  },icd10_sections$range_start,icd10_sections$section))
  
  icd10_sections$level<-levels
  
  icd10_sections
}


process_codes<-function() {
  icd10_codes$code<-as.character(icd10_codes$code)
  icd10_codes$desc<-as.character(icd10_codes$desc)
  
  #   yn_gte<-lapply(icd10_sections$range_start,function(x) x>=icd10_codes$code)
  #   yn_lte<-lapply(icd10_sections$range_start,function(x) x<=icd10_codes$code)
  #   
  #   yn<-mapply(function(x,y) x & y, yn_gte,yn_lte,SIMPLIFY = T)
  #   
  #   
  sects<-lapply(icd10_codes$code,function(x) {
    code<-substring(x,1,3)
    for(i in 1:length(icd10_sections$range_start)) {
      if ((code>=icd10_sections$range_start[i]) & (code<=icd10_sections$range_end[i])  & (icd10_sections$level[i]==1)) {
        return (i)
      }
      
    }
    return (0)
  })
  
  icd10_codes$section<-as.integer(sects)
  icd10_codes
}



