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
#' @param orb_reg indicates the name of the variable storing the information about the region of the entity, for instance NUTS3 in Orbis;
#' used for entity A when its postal code gives no region (code not in the list, or no code)
#' @param cc indicates the name of the variable storing the information about the country code of the entity;
#' the data may hold several countries, and each postal code is looked up in the list of its row's country
#' @param p prefix weight of the Jaro-Winkler similarity used for names, legal forms, cities and streets: 0 (the
#' default) gives the Jaro similarity, as in earlier versions; 0.1 gives the usual Jaro-Winkler similarity, which scores
#' strings with a common start higher (maximum 0.25)
#' @returns an original dataframe with additional variables with similarity scores
#' @examples
#' @export


clean<-function(r,key_a,key_b,name_a,name_b,lf_a,lf_b,zip_a,zip_b,city_a,city_b,street_a,street_b,orb_reg,cc,p=0){

  ##create internal function which separates unique matches from the multiple matches
  ###(base R: data.table's get(key_a) found a column named "key_a" instead of the argument)
  filter<-function(d,key_a,key_b){
    ###number of different A entities matched to each B entity (a missing key counts as one value)
    p<-unique(data.frame(a=d[,key_a],b=d[,key_b]))
    n<-table(p$b,useNA="ifany")
    one<-names(n)[n==1]
    dd<-d[as.character(d[,key_b])%in%one,]
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


  ###compare the original names of matched entities with the Jaro similarity (Jaro-Winkler when p > 0)

  r$name_sim<-stringdist::stringsim(toupper(r[,name_a]),toupper(r[,name_b]),method="jw",p=p)


  ####compare legal forms of the matched entities
  r$lf_sim<-stringdist::stringsim(r[,lf_a],r[,lf_b],method="jw",p=p)
  r$lf_sim<-ifelse(is.na(r$lf_sim),0,r$lf_sim)

  ###compare cities of the matched entities
  r$city_sim<-stringdist::stringsim(toupper(r[,city_a]),toupper(r[,city_b]),method="jw",p=p)
  r$city_sim<-ifelse(is.na(r$city_sim),0,r$city_sim)

  ###and street address
  r$street_sim<-stringdist::stringsim(toupper(r[,street_a]),toupper(r[,street_b]),method="jw",p=p)
  r$street_sim<-ifelse(is.na(r$street_sim),0,r$street_sim)


  ###then the zip codes
  ###look the postal codes up in the list of each row's own country; one call per country for both
  ###columns, so each country's postal code table is built once (rows without a country get no region)
  ccs<-as.character(r[,cc])
  r$reg_a<-rep(NA_character_,nrow(r))
  r$reg_b<-rep(NA_character_,nrow(r))
  for(k in unique(ccs[!is.na(ccs)])){
    i<-which(ccs==k)
    reg<-detect_region(c(r[i,zip_a],r[i,zip_b]),k)
    r$reg_a[i]<-reg[seq_along(i)]
    r$reg_b[i]<-reg[length(i)+seq_along(i)]
  }
  ###no region from the postal code (code not in the list, or no code): use the region given in dataset A
  r$reg_a<-ifelse(is.na(r$reg_a)|r$reg_a=="",r[,orb_reg],r$reg_a)
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
  r$reg_sim<-ifelse(!is.na(r[,zip_a])&!is.na(r[,zip_b])&
                      r[,zip_a]==r[,zip_b],
                    T,
                    r$reg_sim)

  #
  # # ###at the end apply some weights to allow for evaluation of the distances

  return(r)
}

