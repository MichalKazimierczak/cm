#' @title Match records from different sources based on normalized names
#'
#' @description This function matches records from different sources. It implements exact match method
#' @param a first dataset to be matched
#' @param b second dataset to be matched
#' @param aname harmonized name in the first dataset
#' @param bname harmonized name in the second dataset
#' @param sort whether the words in harmonized names should be sorted alphabetically before matching
#' @param unique whether the duplicate names in harmonized names should be eliminated before matching
#' @details The matching key is built from the words of the name: split, sorted (if \code{sort}), repeated words
#' dropped (if \code{unique}), and pasted together without spaces, so that spacing variants match
#' ("AB C" and "A BC" both give "ABC"). Rows with a missing or empty name are left out: they would all share
#' the empty key and match each other.
#' @returns a new dataframe only with matched pairs of names
#' @examples
#' a <- data.frame(id_a = 1:3, n = c("ALPHA BETA", "NOWAK", "ROSSI"))
#' b <- data.frame(id_b = 4:6, n = c("BETA ALPHA", "NOWAK", "BIANCHI"))
#' nmatch(a, b, "n", "n")
#' nmatch(a, b, "n", "n", sort = FALSE)
#' @export


nmatch<-function(a,b,aname,bname,sort=T,unique=T){
  ###work on a plain data frame, so that tibbles and data.tables work too
  a<-as.data.frame(a)
  b<-as.data.frame(b)
  ###Records of a and b match when their names give the same key (exact match): every pair of a record of a
  ###and a record of b with the same key becomes one row of the result

  ###build the key once per distinct name, in one pass: split into words, sort, drop repeated words, paste
  key<-function(x){
    u<-base::unique(x)
    k<-vapply(stringr::str_split(u,"\\s"),function(w){
      if(sort==T) w<-base::sort(w)
      if(unique==T) w<-stringi::stri_unique(w)
      paste(w,collapse="")
    },"")
    k[match(x,u)]
  }
  an<-key(a[,aname])
  bn<-key(b[,bname])

  ###leave out missing and empty names: they would all get the key "" and match each other
  ka<-!is.na(a[,aname])&an!=""
  kb<-!is.na(b[,bname])&bn!=""
  a<-a[ka,,drop=FALSE]
  b<-b[kb,,drop=FALSE]
  an<-an[ka]
  bn<-bn[kb]

  a[,aname]<-NULL
  b[,bname]<-NULL

  a$norm_name<-an
  b$norm_name<-bn

  r<-merge(a,b,by="norm_name")
  return(r)
}

