#' @import stringr

#' @title returns a dataset enriched with additional variables indicating elements of address
#'
#' @description This functions detects different elements of address in the unstructured address field and puts it into separate columns
#' @param r dataset containing freeline address data
#'
#' @param address name of the column containing unstructured address data
#' @param prefix a string that will be addedd to the names of the column with found address elements
#' @param cc indicates a code of a country where the cities A and B are located
#'
#'along with the original version of the name
#' @returns an original dataframe with ordered and ranked matched dataframe




detect_address<-function(r,address,prefix,cc){
  data(postal_codes_regex)
  data(cities)
  r<-data.frame(r)
  pcc<-pc[pc$country==cc,]
  citiesc<-cities[cities$Country.Code==cc,]


  r[,address]<-stringr::str_replace_all(r[,address], "[^\\p{Latin}\\p{Greek}\\p{Cyrillic}0-9]"," ")
  r[,address]<-stringr::str_replace_all(r[,address],"CEDEX"," ")
  r[,address]<-stringr::str_replace_all(r[,address],"\\s+"," ")
  r[,address]<-stringi::stri_trans_general(r[,address], "latin-ascii; upper")

  ###first look for postal codes in the address
  zip<-stringr::str_extract(r[,address],paste0("(?:^| )?[A-Z]?(?:-| )?",pcc$expres))
  r[,address]<-str_replace(r[,address],paste0("(?:^| )?[A-Z]?(?:-| )?",pcc$expres)," ")
  zip<-stringr::str_replace(zip,paste0("(?:^| )?[A-Z]?(?:-| )?",pcc$expres),pcc$forma)
  zip<-ifelse(is.na(zip),"",zip)

  ####Finally detect city information in address
  citiesc<-cities[cities$Country.Code==cc,]
  ###look each distinct address up once (see city_match()), then remove the matched city from it
  u<-unique(r[,address])
  u<-u[!is.na(u)]
  lk<-city_lookup(citiesc)
  m<-city_match(u,lk)
  hit<-which(!is.na(m))
  city<-rep("",length(u))
  city[hit]<-as.character(citiesc$ASCII.Name[m[hit]])
  rest<-u
  ###remove the matched key as a whole-word pattern, as in cities$Alternate.Names
  rest[hit]<-stringr::str_replace(u[hit],paste0("(^| )",lk$key[match(m[hit],lk$row)],"( |$)"),"")
  i<-match(r[,address],u)
  r$city<-city[i]
  r[,address]<-rest[i]
  ####Now create a vector for street information


  r[,paste0(prefix,"_zip")]<-zip
  r[,paste0(prefix,"_city")]<-r$city
  r[,paste0(prefix,"_street")]<-r[,address]

  r$miasto<-NULL
  r$city<-NULL

  return(r)
}
