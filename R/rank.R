#' @import data.table
#' @import stringdist
#' @import stringr
#' @import stringi
#' @import dplyr
#' @import tidyr

#' @title returns a disambiguate match
#'
#' @description This function is being used after the match to correct multiple matches
#' @param r matched dataset to be disambiguated
#'
#' @param name name of the column storing name to be transformed, it has to be a string
#' @param country name of the column storing name of the country of seat
#' @param key name of the column storing unique identifier of the entity
#' @param new_col if TRUE creates new column with normalized version of the name along with the original version of the name
#' @param short if TRUE looks for 'trading as' expressions with the name
#' @param translit if TRUE looks for names written in cyrylic and transforms them into their latin versions
#' @param legal if TRUE looks for legal form expressions in the name and deals with them
#' @returns an original dataframe with two additional copies of records containing 'trading as' expression
#' @examples df<-data.frame(name="Andrzej Beata Celina spółka z ograniczoną odpowiedzialnoscią nazwa skrócona ABC sp. z o.o. SP.J",
#'  country="PL",key=1)
#' @export

#dfn<-norma(df,"name","country","key",new_col=T,short=T,translit=T,legal=T)

bvd_is_root<-1
name_sim<-1
lf_sim<-1
reg_sim<-1
city_sim<-1
street_sim<-4



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

