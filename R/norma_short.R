#' @title Identify and deal with 'trading as' expression in the name
#'
#' @description This function is developed to deal with 'trading as' type of indications in the name. It identifies 'trading as' phrase
#' in different languages and adds copies of each record in which a phrase has been identified: one per part of the name
#' before, between and after the phrases ("A TRADING AS B" gives A and B; "A TRADING AS B TRADING AS C" gives A, B and C).
#' The record with the original name is kept.
#'
#' @param r dataset to be transformed
#' @param name name of the column storing name to be transformed, it has to be a string
#' @returns the original dataframe with the additional copies, without duplicate rows
#' @examples norma_short(df,"name")
#' @export



norma_short<- function(r,name)
{
  ###phrases meaning "trading as" in different languages, one per element, joined into one pattern.
  ##Phrases that can be short or occur inside words have word boundaries (\\b), for example H O D N,
  ##which would otherwise match inside ZACHODNI or WSCHODNIA
  phrases<-c("(?:ALSO)? TRADING AS","(?:TRADING|DOING BUSINESS?) UNDER THE NAME","HANDELND UNTER ?(?:DEN NAMEN)?","\\bC\\/O\\b",
             "\\bT? ?H(?:ANDELENDE)? ?O(?:NDER)? ?DE? ?N(?:AAM)?\\b","OOK TE NOE?MEN","AGISSANT (?:AU NOM DE|COMME)","\\bDITE\\b",
             "\\bDIT\\b","EN ABREGE","OPERANDO COMO","IN SIGLA","(?:O|E)? ?IN FORMA ABBREVI?A?T?A?","(?:O|E)? ?BREVEMENTE",
             "(?:O|E)? ?ABBREV","(?:O|E)? ?DENOMINAZIONE ABBREVIATA","(?:O|E)? ?IN ABBREVIATO",
             "(?:O|E)? ?(?:SIGLABILE|ABBREVIABLE) ?(?:IN)?","(?:O|E)? ?IN BREVE","(?:O|E)? ?ABBREVIATA O SIGLATA","SKROCONA NAZWA:?",
             "NAZWA SKROCONA:?","UZYWA NAZWY SKROCONEJ:?","(?:O|E)? ?DENOM ABBREVIATA","(?:O|E)? ? IN FORMA A",
             "(?:O|E)? ?OPERANTE IN COMERCIO COME","(?:WITH)? ?DISTINCTIVE TITLE","DIAKRITIKOS TITL.OS","DIAKRITIKO TITLO",
             "NOM D ?USAGE","HANDELEND ONDER DE NAAM","\\bHODN\\b","TAMBEM COMERCIANDO COMO")
  short<-paste0("(?:",paste(phrases,collapse="|"),")")

  ###split the names that contain a phrase at every phrase: "A TRADING AS B TRADING AS C" gives A, B and C
  hit<-which(stringr::str_detect(r[,name],short))
  parts<-stringr::str_split(r[hit,name],short)
  n<-lengths(parts)
  ###one extra copy of the record per part, all first parts first, then all second parts, and so on
  k<-sequence(n)
  o<-order(k)
  rn<-r[rep(hit,n)[o],,drop=FALSE]
  rn[,name]<-unlist(parts)[o]

  ###bind together original observations with additional ones created to accomodate new versions of name
  r<-rbind(r,rn)

  ###leave only unique observations
  r<-unique(r)
}
