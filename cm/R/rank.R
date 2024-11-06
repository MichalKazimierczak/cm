#' @import data.table
#' @import stringdist
#' @import stringr
#' @import stringi
#' @import dplyr
#' @import tidyr

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




rankmatch<-function(r,prefer_root=T,bvd_is_root=1,name_sim=1,lf_sim=1,reg_sim=1,city_sim=1,street_sim=1){

  ###standardize the score not to be distorted by the high weights. Now it will always be kept at the range of 0 to 1
  s<-sum(bvd_is_root,name_sim,lf_sim,reg_sim,city_sim,street_sim)
  bvd_is_root<-bvd_is_root/s
  name_sim<-name_sim/s
  lf_sim<-lf_sim/s
  reg_sim<-reg_sim/s
  city_sim<-city_sim/s
  street_sim<-street_sim/s

  r$final_score<-r$bvd_is_root*bvd_is_root+r$name_sim*name_sim+r$lf_sim*lf_sim+r$reg_sim*reg_sim+r$city_sim*city_sim+r$street_sim*street_sim
  if(prefer_root){
    r<-r[order(r$bvd_is_root,r$final_score,decreasing=T),]
  }else{
    r<-r[order(r$final_score,decreasing=T),]
  }

  return(r)
}

