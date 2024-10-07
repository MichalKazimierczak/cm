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

  reg<-rep("",length(pc))

  for (i in 1:nrow(zipc)){
    z<-zipc[i,]
    reg<-ifelse(stringr::str_detect(pc,z$CODE),paste(reg,z$NUTS3,sep=", "),reg)
  }
  reg<-str_remove(reg,"^\\, ")
  return(reg)
}
