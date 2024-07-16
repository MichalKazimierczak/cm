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

  a[,aname]<-stringr::str_split(a[,aname],"\\s")
  b[,bname]<-stringr::str_split(b[,bname],"\\s")


  if(sort==T){
    a[,aname]<-sapply(a[,aname],sort)
    b[,bname]<-sapply(b[,bname],sort)
  }

  a[,aname]<-sapply(a[,aname],function(x) unlist(x))
  b[,bname]<-sapply(b[,bname],function(x) unlist(x))

  if (unique==T){
    a[,aname]<-lapply(a[,aname], function(x) stringi::stri_unique(x))
    b[,bname]<-lapply(b[,bname], function(x) stringi::stri_unique(x))
  }

  a[,aname]<-sapply(a[,aname], function(x) paste(x,collapse=""))
  b[,bname]<-sapply(b[,bname], function(x) paste(x,collapse=""))

  r<-match(a,b,
           by.x="aname",
           by.y="bname")
  return(r)
}

