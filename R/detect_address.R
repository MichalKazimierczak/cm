#' @title Split a free-text address into postal code, city and street
#'
#' @description Finds the postal code (with the country's pattern in \code{postal_codes_regex}) and the city (with
#' the names in \code{cities}) in a free-text address, and puts them into new columns; what is left is the street.
#' The address is cleaned first: Latin, Greek and Cyrillic letters and digits are kept, Latin letters converted to
#' ASCII capitals, \code{CEDEX} removed.
#' @param r dataset containing free-text addresses
#' @param address name of the column containing the address; it is replaced by the cleaned address without postal
#' code and city
#' @param prefix text put before the names of the new columns
#' @param cc ISO 3166-1 code of the country of the addresses (as used in \code{cities}, for example \code{DE}, \code{GR},
#' \code{GB})
#' @returns the dataset with three new columns: \code{<prefix>_zip} (postal code, \code{""} if none found),
#' \code{<prefix>_city} (official name of the city, \code{""} if none found) and \code{<prefix>_street} (the rest)
#' @export

detect_address<-function(r,address,prefix,cc){
  ###read the package data into local variables; data() would load them into the global environment
  pc<-CleanMatch::pc
  cities<-CleanMatch::cities
  ###work on a plain data frame, so that tibbles and data.tables work too
  r<-as.data.frame(r)
  pcc<-pc[pc$country==cc,]
  citiesc<-cities[cities$Country.Code==cc,]


  r[,address]<-stringr::str_replace_all(r[,address], "[^\\p{Latin}\\p{Greek}\\p{Cyrillic}0-9]"," ")
  r[,address]<-stringr::str_replace_all(r[,address],"CEDEX"," ")
  r[,address]<-stringr::str_replace_all(r[,address],"\\s+"," ")
  r[,address]<-stringi::stri_trans_general(r[,address], "latin-ascii; upper")

  ###first look for postal codes in the address
  zip<-stringr::str_extract(r[,address],paste0("(?:^| )?[A-Z]?(?:-| )?",pcc$expres))
  r[,address]<-stringr::str_replace(r[,address],paste0("(?:^| )?[A-Z]?(?:-| )?",pcc$expres)," ")
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
  ###the pattern takes the spaces on both sides, so put one back: the words around the city stay apart
  rest[hit]<-stringr::str_replace(u[hit],paste0("(^| )",lk$key[match(m[hit],lk$row)],"( |$)")," ")
  rest<-stringr::str_squish(rest)
  i<-match(r[,address],u)
  r[,address]<-rest[i]

  ###the results go into new columns only; the other columns of r are left as they are
  r[,paste0(prefix,"_zip")]<-zip
  r[,paste0(prefix,"_city")]<-city[i]
  r[,paste0(prefix,"_street")]<-r[,address]

  return(r)
}
