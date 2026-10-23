#' @title returns a dataset with harmonized names of the cities
#'
#' @description This function allows for proper comparison of the cities' names if they come from different sources
#' @param r dataset containing names of the cities
#'
#' @param city_a indicates a name of the column storing a name of city A
#' @param city_b indicates a name of the column storing a name of city B
#' @param cc ISO 3166-1 code of the country where the cities A and B are located (as used in \code{cities}, for
#' example \code{DE}, \code{GR}, \code{GB})
#' @param new_col if TRUE creates new columns with normalized version of the cities A and B names
#'along with the original version of the name, named \code{<city_a>_harmonized} and \code{<city_b>_harmonized}
#' @returns the dataset as a data frame, with \code{city_a} and \code{city_b} cleaned (capitals, without punctuation)
#' and, with \code{new_col = TRUE}, two new columns with the official city names (the cleaned name when no city is
#' found); with \code{new_col = FALSE} the official names replace \code{city_a} and \code{city_b}
#' @examples
#' r <- data.frame(city_a = c("Muenchen", "Nuernberg", "Berlin-Mitte"), city_b = c("Munich", "Nuremberg", "Berlin"))
#' harmonize_cities(r, "city_a", "city_b", "DE")
#' @export


harmonize_cities<-function(r,city_a,city_b,cc,new_col=T){
  ###work on a plain data frame, so that tibbles and data.tables work too
  r<-as.data.frame(r)
  cities<-CleanMatch::cities
  citiesc<-cities[cities$Country.Code==cc,]
  ###clean each distinct name once and map the result back to the rows
  raw<-unique(c(r[,city_a],r[,city_b]))
  cln<-stringr::str_replace_all(raw, "[^\\p{Latin}\\p{Greek}\\p{Cyrillic}]"," ")
  cln<-stringr::str_replace_all(cln,"\\s+"," ")
  cln<-stringi::stri_trans_general(cln, "latin-ascii; upper")
  cln<-stringr::str_trim(cln)
  r[,city_a]<-cln[match(r[,city_a],raw)]
  r[,city_b]<-cln[match(r[,city_b],raw)]

  ###look each distinct cleaned name up in the city list (see city_match()); keep the name when nothing matches
  u<-unique(c(r[,city_a],r[,city_b]))
  u<-u[!is.na(u)&u!=""]
  m<-city_match(u,city_lookup(citiesc))
  r$city_norm_a<-as.character(citiesc$ASCII.Name)[m[match(r[,city_a],u)]]
  r$city_norm_b<-as.character(citiesc$ASCII.Name)[m[match(r[,city_b],u)]]

  r$city_norm_a<-ifelse(is.na(r$city_norm_a),r[,city_a],r$city_norm_a)
  r$city_norm_b<-ifelse(is.na(r$city_norm_b),r[,city_b],r$city_norm_b)

  if(new_col==T){
    colnames(r)[colnames(r)=="city_norm_a"]<-paste0(city_a,"_harmonized")
    colnames(r)[colnames(r)=="city_norm_b"]<-paste0(city_b,"_harmonized")

  } else{
    r[,city_a]<-r$city_norm_a
    r[,city_b]<-r$city_norm_b
    r$city_norm_a<-NULL
    r$city_norm_b<-NULL

  }


  return(r)
}
