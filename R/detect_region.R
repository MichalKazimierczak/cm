#' @import stringr

#' @title returns a dataset enriched with additional variables indicating region in the country
#'
#' @description This function is using a zip code to return information on the NUTS3 region
#' @param r a dataset containing a column with a zip code
#'
#' @param zip name of the column containing zip code
#' @param cc indicates a code of a country where the cities A and B are located
#'
#'along with the original version of the name
#' @returns an original dataframe with a column describing region added

detect_region<-function(pc,cc){
  data(zip_codes)
  zipc<-zips[zips$country_code==cc,]

  if(nrow(zipc)==0) stop("detect_region: no postal codes for country ",cc)
  if(any(stringr::str_detect(zipc$CODE,"[^A-Z0-9 -]"))) stop("detect_region: postal codes with regex characters")

  ###A code is found in pc when a piece of pc as long as the code equals it, so look those pieces up
  ###in a table instead of testing every code of the country (462,780 for NL) on every value
  u<-unique(as.character(pc))
  u<-u[!is.na(u)]
  rows<-split(seq_len(nrow(zipc)),zipc$CODE)
  hit_id<-integer(0)
  hit_row<-integer(0)
  for(len in unique(nchar(zipc$CODE))){
    k<-pmax(nchar(u)-len+1,0)
    id<-rep(seq_along(u),k)
    st<-sequence(k)
    m<-match(substr(u[id],st,st+len-1),names(rows))
    f<-!is.na(m)
    ###a code can belong to more than one NUTS3 region
    hit_id<-c(hit_id,rep(id[f],lengths(rows[m[f]])))
    hit_row<-c(hit_row,unlist(rows[m[f]],use.names=FALSE))
  }
  ###count each code once per value and paste the regions in the order of zips, as the loop did
  h<-unique(data.frame(id=hit_id,row=hit_row))
  h<-h[order(h$id,h$row),]
  ru<-rep("",length(u))
  if(nrow(h)>0){
    p<-tapply(zipc$NUTS3[h$row],h$id,paste,collapse=", ")
    ru[as.integer(names(p))]<-paste0(", ",p)
  }
  reg<-ru[match(as.character(pc),u)]
  reg<-stringr::str_remove(reg,"^\\, ")
  return(reg)
}
