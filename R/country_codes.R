#' Write country codes as ISO 3166-1 codes, as the package data use them
#'
#' Eurostat writes Greece as EL and the United Kingdom as UK; the package data (cities, zip_codes, the legal forms)
#' use the ISO codes GR and GB. Lower case is accepted too ("de" gives "DE").
#' @param cc vector of country codes
#' @returns the codes in capitals, with EL replaced by GR and UK by GB
#' @noRd

iso_country<-function(cc){
  cc<-toupper(as.character(cc))
  cc[cc%in%"EL"]<-"GR"
  cc[cc%in%"UK"]<-"GB"
  cc
}
