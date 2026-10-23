#' @title returns a dataset with ordered and ranked matches
#'
#' @description This function is being used after the disambiguation of matches to rank possible candidates for the match
#' @param r cleaned dataset to be ranked
#'
#' @param prefer_root logical variable indicating whether the ORBIS observations with the bvd_id number without hyphen should be preferred.
#' Needs a logical or 0/1 column \code{bvd_is_root} in \code{r} (Orbis data; no CleanMatch function creates it). Without that column
#' the term is left out of the score, \code{prefer_root} has no effect, and a message says so
#' @param bvd_is_root weight assigned to the criterion of bvd_id_number not to include hyphen
#' @param name_sim weight assigned to the similarity of original names
#' @param lf_sim weight assigned to the similarity of legal forms
#' @param reg_sim weight assigned to the similarity of seat regions as shown in respective datasets
#' @param city_sim weight assigned to the similarity of cities of seat as shown in respective datasets
#' @param street_sim weight assigned to the similarity of cities of seat as shown in respective datasets
#' @returns the dataframe with a column \code{final_score} (between 0 and 1), sorted by it, highest first
#' (with \code{prefer_root}, root entities first)
#' @examples
#' @export




rankmatch<-function(r,prefer_root=T,bvd_is_root=1,name_sim=1,lf_sim=1,reg_sim=1,city_sim=1,street_sim=1){

  ###bvd_is_root is a column of Orbis data (TRUE when the BvD ID has no hyphen); no other function creates it.
  ###Without the column, its term is left out of the score and prefer_root has no effect
  if(!"bvd_is_root"%in%names(r)){
    message("rankmatch: no column bvd_is_root; the score uses the other similarities only")
    r$bvd_is_root<-0
    bvd_is_root<-0
    prefer_root<-FALSE
    drop_root<-TRUE
  } else drop_root<-FALSE

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

  if(drop_root) r$bvd_is_root<-NULL
  return(r)
}

