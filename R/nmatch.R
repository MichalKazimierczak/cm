#' @title Match records from different sources based on normalized names
#'
#' @description This function matches records from different sources. It implements exact match method
#' @param a first dataset to be matched
#' @param b second dataset to be matched
#' @param aname harmonized name in the first dataset
#' @param bname harmonized name in the second dataset
#' @param sort whether the words in harmonized names should be sorted alphabetically before matching
#' @param unique whether the duplicate names in harmonized names should be eliminated before matching
#' @returns a new dataframe only with matched pairs of names
#' @examples nmatch(x,y,"xname","yname",sort=T)
#' @export


nmatch<-function(a,b,aname,bname,sort=T,unique=T){
  ###This is a dataframe that stores the transliteration standards specific for a given country
  ###of seat. If you want to modify standard of transliteration, modify this dataframe.
  ###More than one standards per country are possible. This will multiply rows by number of
  ###standards use to transliterate names

  an<-a[,aname]
  bn<-b[,bname]

  an<-stringr::str_split(an,"\\s")
  bn<-stringr::str_split(bn,"\\s")


  if(sort==T){
    an<-lapply(an,sort)
    bn<-lapply(bn,sort)
  }

  an<-lapply(an,function(x) unlist(x))
  bn<-sapply(bn,function(x) unlist(x))

  if (unique==T){
    an<-lapply(an, function(x) stringi::stri_unique(x))
    bn<-lapply(bn, function(x) stringi::stri_unique(x))
  }

  an<-sapply(an, function(x) paste(x,collapse=""))
  bn<-sapply(bn, function(x) paste(x,collapse=""))

  a[,aname]<-NULL
  b[,bname]<-NULL

  a$norm_name<-an
  b$norm_name<-bn

  r<-merge(a,b,by="norm_name")
  return(r)
}

