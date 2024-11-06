#' @title This function deals with legal forms indications present in the names
#'
#' @description This function is developed to deal with the legal forms expressions found in the names
#' Treatment is country specific and the algorithm is not dealing with legal forms, which may
#' be associated with countries other than countries of seat.
#' @param r dataset to be transformed
#' @param name name of the column storing name to be transformed, it has to be a string
#' @param country name of the column storing country of seat, it has to be a string
#' @returns an original dataframe with cleaned version of original name and additional column with standardized version of legal form
#' @examples norma_legal(df,"name","country_code")
#' @export

norma_legal <- function(r,name,country){
  x<-r
  ###Pre clean data before cleaning legal forms
  x[,name] <- stringr::str_replace_all(x[,name],"GESELL?SC?HAFT"," GESELLSCHAFT")
  x[,name] <- stringr::str_replace_all(x[,name],"(GESELL?SC?HAFT (?:FUE?R|ZUE?R)?)?(.*)(GES M B H|GES MBH|GESMBH|G ?M ?B ?H)","\\2  GMBH")
  x[,name] <- stringr::str_replace_all(x[,name],"GES ?G ?M ?B ?H"," GMBH")
  x[,name] <- stringr::str_replace_all(x[,name],"AKTIEN ?GESELL?SC?HAFT"," AKTIENGESELLSCHAFT")
  x[,name] <- stringr::str_replace_all(x[,name],"KOMM?ANDIT ?(ERWERBS?)? ?GESELL?SC?HAFT"," KOMMANDITGESELLSCHAFT")
  x[,name] <- stringr::str_replace_all(x[,name],"PARTNERSCHAFTS? GESELL?SC?HAFT"," PARTNERSCHAFTSGESELLSCHAFT")
  x[,name] <- stringr::str_replace_all(x[,name],"OFFENE ?(ERWERBS?)? ?GESELL?SC?HAFT"," OFFENE GESELLSCHAFT")
  x[,name] <- stringr::str_replace_all(x[,name],"EINZEL GESELL?SC?HAFT"," EINZELGESELLSCHAFT")
  x[,name] <- stringr::str_replace_all(x[,name],"(.*)(?:^A(?:KTIE)? ?B(?:OLAGE?T?)? |A(?:KTIE)? ?B(?:OLAGE?T?)? |A(?:KTIE)? ?B(?:OLAGE?T?)?$)(.*)(?:PUBL)","\\1 \\2 AB PUBL")
  # x[,name] <- stringr::str_replace_all(x[,name],"(.*)(?:(?:ANONI?IMI|ANONYMI|ANONYMOU?S|ANONIMOS).+?(?:.ETERII?A|ETAII?RE?I?I?A| E$| ET$|\\bETAII\\b))(.*)","\\1 \\2 AE")
  # x[,name] <- stringr::str_replace_all(x[,name],"(.*)(?:(?:\\bOM\\b|\\bOMOR\\b|\\bOMORR?.THMOS\\b|\\bOMORR?.THMI\\b| O|^O).+?(?:ETERII?A|ETAII?RE?I?I?A))(.*)","\\1 \\2 OE")
  x[,name] <- stringr::str_replace_all(x[,name],"GMBH(?:\\+|UND|&)(.*)","GMBH UND \\1")
  x[,name] <- stringr::str_replace_all(x[,name],"\\b3 D\\b","3D")
  ###create a data frame which will store transformed data
  r<-data.frame()

  ###read list of regex expressions that store the legal forms specific for EU MS countries
  data(legal_forms_regex)
  l<-l[order(l$order),]
  # l<-read.csv("legal_forms.csv",colClasses=
  #               c("character","character","character","integer"))
  data(weak_words_regex)
  w<-w[order(w$order),]

  ###As Benelux countries have one composed IPR register in some data sets we have to foresee special treatment of those countries,

  if("BX"%in%unique(x[,country])){

    lb<-l[l$Country%in%c("BE","NL","LU"),]
    lb$Country<-"BX"
    l<-rbind(l,lb)
    wb<-w[w$Country%in%c("BE","NL","LU"),]
    wb$Country<-"BX"
    w<-rbind(w,wb)
    remove(wb,lb)
  }

  # ###read list of regex expressions that store the second list of legal forms, that helps clean composed legal forms
  # ###specific for some countries
  # l2<-read.csv("legal_forms_second_step.csv", colClasses=
  #                c("character","character","character","integer"))

  ###create a list of legal forms limited only to countries present in a dataset
  lis_co <- l[l$Country%in%unique(x[,country]),]
  ###and then loop over all countries present in this dataset
  for (i in unique(lis_co$Country))
  {
    ###create a list of legal forms limited to i country
    co<-lis_co[lis_co$Country==i,]
    ###and a list of weak words limited to i country
    cw<-w[w$Country==i,]

    ###a dataset with observations limited to i country
    cd<-x[x[,country]==i,]
    cd$lf<-""
    ##and a dataset for storing cleaned data for i country
    #cl<-data.frame()
    ###Then loop over a list of legal forms specific for i country
    rep<-0
    repeat {

      for (n in 1:nrow(co))
      {
        ###create a new column with standardized legal form information in case we found legal form expression
        cd$lf<-ifelse(stringr::str_detect(cd[,name],as.character(co[n,"to.replace"])),
                      paste(cd$lf,co[n,"replacement"],"; "),
                      cd$lf)
        ###replace legal form expression from the name
        cd[,name]<-ifelse(stringr::str_detect(cd[,name],as.character(co[n,"to.replace"])),
                          stringr::str_replace(cd[,name],as.character(co[n,"to.replace"])," "),
                          cd[,name])

      }

      rep=rep+1
      if (rep==2){
        break
      }
    }
    for (n in 1:nrow(cw))
    {

      ###replace legal form expression from the name
      cd[,name]<-ifelse(stringr::str_detect(cd[,name],as.character(cw[n,"to.replace"])),
                        stringr::str_replace(cd[,name],as.character(cw[n,"to.replace"]),cw[n,"replacement"]),
                        cd[,name])

    }
    r<-rbind(r,cd)

  }

  ###do some additional cleaning for
  r[,name]<-stringr::str_replace_all(r[,name],"\\s+"," ")
  r[,name]<-stringr::str_trim(r[,name])

  ##and return r
  return(r)
}
