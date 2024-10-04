#' @import data.table
#' @import stringr

#' @title returns a dataset with harmonized names of the cities
#'
#' @description This function allows for proper comparison of the cities' names if they come from different sources
#' @param r dataset containing names of the cities
#'
#' @param city_a indicates a name of the column storing a name of city A
#' @param city_b indicates a name of the column storing a name of city B
#' @param cc indicates a code of a country where the cities A and B are located
#' @param new_col if TRUE creates new columns with normalized version of the cities A and B names
#'along with the original version of the name
#' @returns an original dataframe with ordered and ranked matched dataframe
#' @examples
#' @export


harmonize_cities<-function(r,city_a,city_b,cc,new_col=T){
  data(cities)
  citiesc<-cities[cities$Country.Code==cc,]
  r[,city_a]<-stringr::str_replace_all(r[,city_a], "[^a-zA-Zα-ωΑ-Ωa-яA-Я]"," ")
  r[,city_a]<-stringr::str_replace_all(r[,city_a],"\\s+"," ")
  r[,city_b]<-stringr::str_replace_all(r[,city_b], "[^a-zA-Zα-ωΑ-Ωa-яA-Я]"," ")
  r[,city_b]<-stringr::str_replace_all(r[,city_b],"\\s+"," ")
  r[,city_a]<-stringi::stri_trans_general(r[,city_a], "latin-ascii; upper")
  r[,city_b]<-stringi::stri_trans_general(r[,city_b], "latin-ascii; upper")
  r[,city_a]<-stringr::str_trim(r[,city_a])
  r[,city_b]<-stringr::str_trim(r[,city_b])
  r$city_norm_a<-""
  r$city_norm_b<-""
  for (i in 1:nrow(citiesc)){
    cit<-as.character(citiesc[i,"Alternate.Names"])
    r$city_norm_a<-ifelse(r$city_norm_a==""&stringr::str_detect(r[,city_a],cit),as.character(citiesc[i,"ASCII.Name"]),r$city_norm_a)
    r$city_norm_b<-ifelse(r$city_norm_b==""&stringr::str_detect(r[,city_b],cit),as.character(citiesc[i,"ASCII.Name"]),r$city_norm_b)
  }

  r$city_norm_a<-ifelse(r$city_norm_a=="",r[,city_a],r$city_norm_a)
  r$city_norm_b<-ifelse(r$city_norm_b=="",r[,city_b],r$city_norm_b)

  if(new_col==T){
    cla<-paste0(colnames(r)[str_detect(city_a,colnames(r))],"_harmonized")
    colnames(r)[str_detect(colnames(r),"city_norm_a")]<-cla
    clb<-paste0(colnames(r)[str_detect(city_b,colnames(r))],"_harmonized")
    colnames(r)[str_detect(colnames(r),"city_norm_b")]<-clb

  } else{
    r[,city_a]<-r$city_norm_a
    r[,city_b]<-r$city_norm_b
    r$city_norm_a<-NULL
    r$city_norm_b<-NULL

  }


  return(r)
}
