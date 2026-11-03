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
#' \code{GB}); Eurostat's \code{EL} and \code{UK} and lower case work too
#' @details When an address holds several place names (a street named after a town, a district after the city),
#' the city is the place that follows the postal code most closely, or the one just before the code when none
#' follows it (\code{London SW1A 2AA}). Without a postal code, a place directly after a street word (\code{UL},
#' \code{RUE}, \code{VIA} and others, see \code{CleanMatch:::street_words}) comes last; otherwise official names,
#' longer names and larger places come first, as in \code{\link{harmonize_cities}}.
#' @returns the dataset with three new columns: \code{<prefix>_zip} (postal code, \code{""} if none found),
#' \code{<prefix>_city} (official name of the city, \code{""} if none found) and \code{<prefix>_street} (the rest)
#' @examples
#' r <- data.frame(adr = c("Marienplatz 1, 80331 Muenchen", "Unter den Linden 5, D-10117 Berlin"))
#' detect_address(r, "adr", "x", "DE")
#' @export

detect_address<-function(r,address,prefix,cc){
  ###read the package data into local variables; data() would load them into the global environment
  pc<-CleanMatch::pc
  cities<-CleanMatch::cities
  ###work on a plain data frame, so that tibbles and data.tables work too
  r<-as.data.frame(r)
  ###country codes as in the package data: EL (Eurostat) is GR, UK is GB, lower case accepted
  cc<-iso_country(cc)
  pcc<-pc[pc$country==cc,]
  citiesc<-cities[cities$Country.Code==cc,]


  r[,address]<-stringr::str_replace_all(r[,address], "[^\\p{Latin}\\p{Greek}\\p{Cyrillic}0-9]"," ")
  r[,address]<-stringr::str_replace_all(r[,address],"\\s+"," ")
  r[,address]<-stringi::stri_trans_general(r[,address], "latin-ascii; upper")
  ###remove CEDEX (French business mail) and its number, after upper-casing so that "Cedex 01" is found too
  r[,address]<-stringr::str_squish(stringr::str_replace_all(r[,address],"\\bCEDEX(?: [0-9]{1,2})?\\b"," "))

  ###first look for postal codes in the address; leave a marker where the code was, so that its position
  ###can help to choose the city (the marker has digits, and no city name has, so it never matches a city)
  marker<-"0ZIP0"
  zip<-rep(NA_character_,nrow(r))
  if(nrow(pcc)>0){
    ###an optional country letter before the code (D-80331, L 1234) must stand alone: before, it could take the
    ###last letter of the word in front (LONDON SW1A 2AA lost the N of LONDON)
    ###it is tried only when the code does not match without it (SW1A 2AA is a code, not S + W1A 2AA)
    zp<-paste0("(?:\\b[A-Z](?:-| )?)??",pcc$expres)
    zip<-stringr::str_extract(r[,address],zp)
    r[,address]<-stringr::str_replace(r[,address],zp,paste0(" ",marker," "))
    zip<-stringr::str_replace(zip,zp,pcc$forma)
  }
  zip<-ifelse(is.na(zip),"",zip)

  ####Finally detect city information in address
  ###look each distinct address up once and find all place names in it (see city_hits())
  u<-unique(r[,address])
  u<-u[!is.na(u)]
  lk<-city_lookup(citiesc)
  h<-city_hits(u,lk)
  w<-strsplit(u," ",fixed=TRUE)
  start<-c(0L,cumsum(lengths(w)))[seq_along(u)]
  word<-unlist(w,use.names=FALSE)
  ###choose one place per address:
  ###- with a postal code: the place that follows the code most closely; if none follows, the one just before it
  ###- without one: a place directly after a street word (UL LESNA, RUE DE PARIS) comes last
  ###- otherwise, as in harmonize_cities(): official names first, then longer names, then larger places
  zat<-vapply(w,function(x){z<-which(x==marker); if(length(z)) z[1] else NA_integer_},1L)[h$id]
  prev<-ifelse(h$from>1,word[start[h$id]+h$from-1],"")
  score<-ifelse(is.na(zat),ifelse(prev%in%street_words,1,0),
                ifelse(h$from>zat,h$from-zat,1000+zat-h$to))
  h<-h[order(h$id,score,h$row),]
  h<-h[!duplicated(h$id),]
  city<-rep("",length(u))
  city[h$id]<-as.character(citiesc$ASCII.Name[h$row])
  ###the street is what is left: drop the words of the chosen place and the marker
  keep<-word!=marker
  drop<-unlist(Map(function(s,f,t) s+f:t,start[h$id],h$from,h$to),use.names=FALSE)
  keep[drop]<-FALSE
  rest<-vapply(split(word[keep],factor(rep(seq_along(u),lengths(w))[keep],levels=seq_along(u))),paste,"",collapse=" ")
  rest<-stringr::str_squish(rest)
  i<-match(r[,address],u)
  r[,address]<-rest[i]

  ###the results go into new columns only; the other columns of r are left as they are
  r[,paste0(prefix,"_zip")]<-zip
  r[,paste0(prefix,"_city")]<-city[i]
  r[,paste0(prefix,"_street")]<-r[,address]

  return(r)
}

## Words that start a street name in common address formats (after cleaning: capitals, no full stops). A place name
## directly after one of them is most likely the street (UL LESNA is a street in Poland; LESNA is also a town).
street_words<-c("UL","ULICA","AL","ALEJA","PL","PLAC","OS","OSIEDLE","RUE","AVENUE","AV","BD","BOULEVARD","CHEMIN",
                "ALLEE","IMPASSE","QUAI","VIA","VIALE","CORSO","PIAZZA","PIAZZALE","VICOLO","CALLE","AVENIDA","PLAZA",
                "PASEO","CARRER","RUA","TRAVESSA","LARGO","STRADA","BULEVARDUL","STR","ULITSA","ODOS","LEOFOROS",
                "STRAAT","LAAN","WEG","PLEIN")
