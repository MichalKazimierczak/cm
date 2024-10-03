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

setwd("Y:/ESS/IP impact/2024_firm_level/data/matched/plain/enriched")
r<-readRDS("AT_orbis_euipo.rds")
r$key_a<-r$bvd_id_number
r$key_b<-r$OWNER_CODE
r$name_a<-r$name
r$name_b<-r$OWNER_NAME
r$lf_a<-r$lf.x
r$lf_b<-r$lf.y
r$zip_a<-r$orb_postcode
r$zip_b<-r$euipo_postcode
r$city_a<-r$orb_city
r$city_b<-r$euipo_city



clean<-function(r,key_a,key_b,name_a,name_b,lf_a,lf_b,zip_a,zip_b,city_a,city_b,street_a,street_b,orb_reg,cc){

  ##create internal function which separates unique matches from the multiple matches
  filter<-function(d,key_a,key_b){
    st<-data.table::data.table(d)[,.(n=uniqueN(get(key_a))),by=key_b]
    st<-data.frame(st)
    dd<-d[d[,key_b]%in%st[st$n==1,key_b],]
    return (data.frame(dd))
  }

  r<-data.frame(r)


  ###In the first step separate unique matches from the multiple matches
  ###create a separate function for that as it will be constantly used
  rd<-filter(r,key_a,key_b)
  rd$one_to_one<-T
  if(nrow(rd)!=nrow(r)){
    rn<-r[!r[,key_b]%in%rd[,key_b],]
    rn$one_to_one<-F
    #length(unique(rd$key_b))

    r<-rbind(rd,rn)
  } else {
    r<-rd
  }


  ###compare the Leventshein distance between original names of matched entities

  r$name_sim<-stringdist::stringsim(toupper(r[,name_a]),toupper(r[,name_b]),method="jw")


  ####compare legal forms of the matched entities
  r$lf_sim<-stringdist::stringsim(r[,lf_a],r[,lf_b],method="jw")
  r$lf_sim<-ifelse(is.na(r$lf_sim),0,r$lf_sim)

  ###compare cities of the matched entities
  r$city_sim<-stringdist::stringsim(toupper(r[,city_a]),toupper(r[,city_b]),method="jw")
  r$city_sim<-ifelse(is.na(r$city_sim),0,r$city_sim)
  r$city_sim<-ifelse(is.na(r$city_sim),0,r$city_sim)

  ###and street address
  r$street_sim<-stringdist::stringsim(toupper(r[,street_a]),toupper(r[,street_b]),method="jw")
  r$street_sim<-ifelse(is.na(r$street_sim),0,r$street_sim)
  r$street_sim<-ifelse(is.na(r$street_sim),0,r$street_sim)

  ###then the zip codes
  iso<-unique(data.frame(r[,"iso2"]))
  #zip<-data.table::fread(paste0("https://gisco-services.ec.europa.eu/tercet/NUTS-2024/pc2024_",iso,"_NUTS-2024_v1.0.zip"))
  data(zip_codes)
  zip<-data.table::fread(paste0("C:/Users/KAZIMMI/Downloads/pc2024_NUTS-2024_v2.0/",zip))




  r<-data.frame(r)
  r<-merge(r,zip,by.x=zip_a,by.y="CODE",all.x=T)
  colnames(r)[length(r)]<-"reg_a"
  r$reg_a<-ifelse(!is.na(r[,orb_reg]),r[,orb_reg],r$reg_a)
  r<-merge(r,zip,by.x=zip_b,by.y="CODE",all.x=T)
  colnames(r)[length(r)]<-"reg_b"

  #
  r$reg_sim<-ifelse(!is.na(r$reg_a)&!is.na(r$reg_b)&r$reg_a==r$reg_b,T,F)

  ####As there may be some old postcodes that are no longer associated with regions, we give the possibility to assign
  ###True to region similarity if two entities have the same postal code
  r$reg_sim<-ifelse(!r$reg_sim&
                      !is.na(r[,zip_a])&
                      !is.na(r[,zip_b])&
                      r[,zip_a]==r[,zip_b],
                    T,
                    F)

  #
  # # ###at the end apply some weights to allow for evaluation of the distances

  return(r)
}

