#' @title Identify and deal with 'trading as' expression in the name
#'
#' @description This function is developed to deal with 'trading as' type of indications in the name. It identifies 'trading as' phrase
#' in different languages and creates three copies of record, for which this phrase has been identified: original version of name
#' a string preceding 'trading as' expression and a string following 'trading as' expression.
#'
#' @param r dataset to be transformed
#' @param name name of the column storing name to be transformed, it has to be a string
#' @returns an original dataframe with two additional copies of records containing 'trading as' expression
#' @examples norma_short(df,"name")
#' @export



norma_short<- function(r,name)
{
  ##short contains 'trading as' type of phrases identified in different languages
  short<-"(:?(?:ALSO)? TRADING AS|(?:TRADING|DOING BUSINESS?) UNDER THE NAME|HANDELND UNTER ?(?:DEN NAMEN)?|\\bC\\/O\\b|
T? ?H(?:ANDELENDE)? ?O(?:NDER)? ?DE? ?N(?:AAM)?|OOK TE NOE?MEN|AGISSANT (?:AU NOM DE|COMME)|\\bDITE\\b|\\bDIT\\b|EN ABREGE|
OPERANDO COMO|IN SIGLA|(?:O|E)? ?IN FORMA ABBREVI?A?T?A?|(?:O|E)? ?BREVEMENTE|(?:O|E)? ?ABBREV|(?:O|E)? ?DENOMINAZIONE ABBREVIATA|
(?:O|E)? ?IN ABBREVIATO|(?:O|E)? ?(?:SIGLABILE|ABBREVIABLE) ?(?:IN)?|(?:O|E)? ?IN BREVE|(?:O|E)? ?ABBREVIATA O SIGLATA|SKROCONA NAZWA:?|NAZWA SKROCONA:?|UZYWA NAZWY SKROCONEJ:?|(?:O|E)? ?DENOM ABBREVIATA|(?:O|E)? ? IN FORMA A|(?:O|E)? ?OPERANTE IN COMERCIO COME|
(?:WITH)? ?DISTINCTIVE TITLE|D ?T|DIAKRITIKOS TITL.OS|DIAKRITIKO TITLO|NOM D ?USAGE|HANDELEND ONDER DE NAAM|\bHODN\b|TAMBEM COMERCIANDO COMO)"
  ###create a column to store the part of string preceding 'trading as' expression
  r$name_1<-ifelse(str_detect(r[,name],short),
                   str_replace(r[,name],paste0("(.*)",short,"(.*)"),"\\1"),
                   NA)
  ###create a column to store the part of string following 'trading as' expression
  r$name_2<-ifelse(str_detect(r[,name],short),
                   str_replace(r[,name],paste0("(.*)",short,"(.*)"),"\\3"),
                   NA)

  ###create new datasets for those observations where we were able to identify 'trading as' types of expressions
  rn1<-r[!is.na(r$name_1),]
  rn2<-r[!is.na(r$name_2),]

  ###convert new columns created for new names into old 'name' column
  rn1[,name]<-rn1$name_1
  rn2[,name]<-rn2$name_2

  ###bind together original observations with additional ones created to accomodate new versions of name
  r<-rbind(r,rn1,rn2)

  ###delete additional columns created to store additional versions of name
  r$name_1<-NULL
  r$name_2<-NULL
  ###leave only unique observations
  r<-unique(r)
}
