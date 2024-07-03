#' @title Transliterate characters other than Latin
#'
#' @description This function is developed to transliterate the strings in other alphabets than Latin
#' @param r dataset to be transformed
#' @param name name of the column storing name to be transformed, it has to be a string
#' @param country name of the column storing country of seat, it has to be a string
#' @param key name of the column storing key id, it has to be a string
#' @returns an original dataframe with transliterated version of name
#' @examples norma_trans(df,"name","country_code")
#' @export


norma_trans<-function(r,name,country,key){
  ###This is a dataframe that stores the transliteration standards specific for a given country
  ###of seat. If you want to modify standard of transliteration, modify this dataframe.
  ###More than one standards per country are possible. This will multiply rows by number of
  ###standards use to transliterate names
  trans<-data.frame(country=c("BG","CY","GR"),
                    standard=c("bg-bg_Latn/BGN","Greek-Latin/BGN","Greek-Latin/BGN"))

  ##keep only those observations from BG, CY and GR and where names are written in cyrylic
  r_trans<-r[r[,country]%in%c("BG","CY","GR")&!str_detect(r[,name],"[A-Z]"),]
  if(nrow(r_trans!=0)){
    ###create a dataset to store transformed names
    r_tr<-data.frame()
    for (k in 1:nrow(trans))
    {
      r_t<-r_trans[r_trans[,country]==as.character(trans[k,"country"]),]
      r_t[,name]<-stri_trans_general(r_t[,name],as.character(trans[k,"standard"]))
      if(as.character(trans[k,"country"])=="BG"){
        r_t[,name]<-str_replace_all(r_t[,name],"KH","H")
        r_t[,name]<-str_replace_all(r_t[,name],"IY","I")
        r_t[,name]<-str_replace_all(r_t[,name],"YU|IU|JU","U")
        r_t[,name]<-str_replace_all(r_t[,name],"YA|JA","A")
      }
      r_tr<-rbind(r_tr,r_t)

    }
    ###create a dataset with observations that were not transformed
    r_n<-r[!r[,key]%in%r_trans[,key],]
    ###and bind transformed and not transformed observations
    r<-rbind(r_tr,r_n)
    return(unique(r))
  }
  else{
    return(r)
  }
}
