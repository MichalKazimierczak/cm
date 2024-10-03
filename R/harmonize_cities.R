#' @import data.table
#' @import stringr

#' @title returns a dataset with ordered and ranked matches
#'
#' @description This function is being used after the disambiguation of matches to rank possible candidates for the match
#' @param r cleaned dataset to be ranked
#'
#' @param prefer_root logical variable indicating whether the ORBIS observations with the bvd_id number without hyphen should be preferred
#' @param bvd_is_root weight assigned to the criterion of bvd_id_number not to include hyphen
#' @param name_sim weight assigned to the similarity of original names
#' @param lf_sim weight assigned to the similarity of legal forms
#' @param reg_sim weight assigned to the similarity of seat regions as shown in respective datasets
#' @param city_sim weight assigned to the similarity of cities of seat as shown in respective datasets
#' @param street_sim weight assigned to the similarity of cities of seat as shown in respective datasets
#' @returns an original dataframe with ordered and ranked matched dataframe
#' @examples
#' @export


harmonize_cities<-function(r,city_a,city_b,cc){
  data(cities)
  citiesc<-cities[cities$Country.Code==cc,]
  r[,city_a]<-stringr::str_replace_all(r[,city_a], "[^a-zA-Zα-ωΑ-Ωa-яA-Я]"," ")
  r[,city_a]<-stringr::str_replace_all(r[,city_a],"\\s+"," ")
  r[,city_b]<-stringr::str_replace_all(r[,city_b], "[^a-zA-Zα-ωΑ-Ωa-яA-Я]"," ")
  r[,city_b]<-stringr::str_replace_all(r[,city_b],"\\s+"," ")
  r[,city_a]<-stringi::stri_trans_general(r[,city_a], "latin-ascii; upper")
  r[,city_b]<-stringi::stri_trans_general(r[,city_b], "latin-ascii; upper")
  r[,city_a]<-str_trim(r[,city_a])
  r[,city_b]<-str_trim(r[,city_b])
  r$city_norm_a<-""
  r$city_norm_b<-""
  for (i in 1:nrow(citiesc)){
    cit<-citiesc[i,"alt_names"]
    r$city_norm_a<-ifelse(r$city_norm_a==""&stringr::str_detect(r[,city_a],cit),as.character(citiesc[i,"ASCII.Name"]),r$city_norm_a)
    r$city_norm_b<-ifelse(r$city_norm_b==""&stringr::str_detect(r[,city_b],cit),as.character(citiesc[i,"ASCII.Name"]),r$city_norm_b)
    print(i)
  }


  return(r)
}

