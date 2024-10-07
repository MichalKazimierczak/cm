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
#' @examples
#' @export


detect_address<-function(r,address,prefix,cc){
  data(postal_codes_regex)
  data(zip_codes)
  data(cities)

  pcc<-pc[pc$country==cc,]
  citiesc<-cities[cities$Country.Code==cc,]
  zipc<-zips[zips$country_code==cc,]

  r[,address]<-stringr::str_replace_all(r[,address], "[^a-zA-Zα-ωΑ-Ωa-яA-Я0-9]"," ")
  r[,address]<-stringr::str_replace_all(r[,address],"\\s+"," ")
  r[,address]<-stringi::stri_trans_general(n$noa_city, "latin-ascii; upper")

  ###first look for postal codes in the address
  zip<-stringr::str_extract(r[,address],paste0("(?:^| )?[A-Z]?(?:-| )?",pcc$expres))
  r[,address]<-str_replace(r[,address],paste0("(?:^| )?[A-Z]?(?:-| )?",pcc$expres)," ")
  zip<-stringr::str_replace(zip,paste0("(?:^| )?[A-Z]?(?:-| )?",pcc$expres),pcc$forma)
  zip<-ifelse(is.na(zip),"",zip)

  ####Now add information about the region based on extracted postal codes
  for (i in 1:nrow(zipc)){
    z<-zipc[i,]
    reg<-ifelse(stringr::str_detect(zip,z$CODE),z$NUTS3,"")
  }

  ####Finally detect city information in address
  citiesc<-cities[cities$Country.Code==cc,]
  r$city<-rep("",nrow(r))

  for (i in 1:nrow(citiesc)){
    cit<-as.character(citiesc[i,"Alternate.Names"])
    r$miasto<-ifelse(r$city==""&stringr::str_detect(r[,address],cit),cit,"")
    r$city<-ifelse(r$miasto!="",as.character(citiesc[i,"ASCII.Name"]),r$city)
    r[,address]<-ifelse(r$miasto!="",str_replace(r[,address],r$miasto,""),r[,address])
    r$miasto==""
  }
  ####Now create a vector for street information


  r[,paste0(prefix,"_zip")]<-zip
  r[,paste0(prefix,"_region")]<-reg
  r[,paste0(prefix,"_city")]<-r$city
  r[,paste0(prefix,"_street")]<-r[,address]

  r$miasto<-NULL
  r$city<-NULL

  return(r)
}
