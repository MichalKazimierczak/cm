#' @import data.table
#' @import stringdist
#' @import stringr
#' @import stringi


#' @title returns a disambiguated match
#'
#' @description This function is being used after the match, to correct multiple matches. It uses additional features such as
#' original names, address and legal form information to assign similarity scores
#' @param r matched dataset to be disambiguated
#'
#' @param key_a indicates the name of the variable storing the identifier of the A dataset
#' @param key_b indicates the name of the variable storing the identifier of the B dataset
#' @param name_a indicates the name of the variable storing the original name before the normalization as stored in dataset A
#' @param name_b indicates the name of the variable storing the original name before the normalization as stored in dataset B
#' @param lf_a indicates the name of the variable storing the legal forms extracted from the name of entity stored in dataset A
#' @param lf_b indicates the name of the variable storing the legal forms extracted from the name of entity stored in dataset B
#' @param zip_a indicates the name of the variable storing the postal codes of the entity stored in dataset A
#' @param zip_b indicates the name of the variable storing the postal codes of the entity stored in dataset B
#' @param city_a indicates the name of the variable storing the name of the city of the entity stored in dataset A
#' @param city_b indicates the name of the variable storing the name of the city of the entity stored in dataset B
#' @param zip_a indicates the name of the variable storing the postal code of the entity stored in dataset A
#' @param zip_b indicates the name of the variable storing the postal code of the entity stored in dataset B
#' @param street_a indicates the name of the variable storing the street address of the entity stored in dataset A
#' @param street_b indicates the name of the variable storing the street address of the entity stored in dataset B
#' @param orb_reg indicates the name of the variable storing the information about the region of the entity, for instance NUTS3 in Orbis
#' @param cc indicates the name of the variable storing the information about the country code of the entity
#' @returns an original dataframe with additional variables with similarity scores
#' @examples
#' @export


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
  r$reg_a<-detect_region(r[,zip_a],cc)
  r$reg_b<-detect_region(r[,zip_b],cc)
  # iso<-unique(data.frame(r[,"iso2"]))
  # data(zip_codes)
  # zip<-zips[zips$country_code==cc,]
  # r<-data.frame(r)
  # r<-merge(r,zip,by.x=zip_a,by.y="CODE",all.x=T)
  # colnames(r)[length(r)]<-"reg_a"
  # r$reg_a<-ifelse(r$reg_a=="",r[,orb_reg],r$reg_a)
  # r<-merge(r,zip,by.x=zip_b,by.y="CODE",all.x=T)
  # colnames(r)[length(r)]<-"reg_b"

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

