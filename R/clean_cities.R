#' @import data.table


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

harmonize_cities(r,orb_city,euipo_city,"AT")


harmonize_cities<-function(r,city_a,city_b,cc){
  data(cities)
  citiesc<-cities[cities$Country.Code==cc,]
  r$norm_city_a<-""
  r$norm_city_b<-""
  for (i in 1:nrow(citiesc)){
    cit<-paste0("(^| )",as.character(citiesc[i,"Alternate.Names"]),"( |$)")
    r$norm_city_a<-ifelse(r$norm_city_a==""&str_detect(r[,city_a],cit),as.character(citiesc[i,"ASCII.Name"]),r$city_norm_a)
    r$norm_city_b<-ifelse(r$norm_city_b==""&str_detect(r[,city_b],cit),as.character(citiesc[i,"ASCII.Name"]),r$city_norm_b)
  }


  return(r)
}

