#' @title returns an original dataset with normalized name
#'
#' @description This is the main function of the package
#' @param r dataset to be transformed
#' @param name name of the column storing name to be transformed, it has to be a string
#' @param country name of the column storing name of the country of seat
#' @param key name of the column storing unique identifier of the entity
#' @param new_col if TRUE creates new column with normalized version of the name along with the original version of the name
#' @param short if TRUE looks for 'trading as' expressions with the name
#' @param translit if TRUE transliterates names of BG, CY and GR entities written in Cyrillic or Greek into Latin
#' @param legal if TRUE looks for legal form expressions in the name and deals with them
#' @returns the dataset as a data frame with the normalised name in \code{norm_name} (or in \code{name} with
#' \code{new_col = FALSE}) and, with \code{legal = TRUE}, the standardised legal forms in \code{lf} (several separated
#' by \code{;}). With \code{short = TRUE}, records with a 'trading as' phrase appear more than once, once per part of
#' the name
#' @examples
#' df <- data.frame(key = 1:3, country = c("PL", "DE", "BG"),
#'                  name = c("Nowak spolka z ograniczona odpowiedzialnoscia nazwa skrocona NWK sp. z o.o.",
#'                           "Mueller Baeckerei GmbH & Co. KG", "Maya Khristov Trading OOD"))
#' norma(df, "name", "country", "key")
#' @export

#dfn<-norma(df,"name","country","key",new_col=T,short=T,translit=T,legal=T)

norma<-function(r,name,country,key,new_col=T,short=T,translit=T,legal=T){
  ###This is to account for the choice of whether to create a new column for transliterated names
  ##or to transform the column which stored the original name
  ###work on a plain data frame, so that tibbles and data.tables work too
  r<-as.data.frame(r)
  r$norm_name<-toupper(r[,name])
  r$norm_name <-stringi::stri_enc_toutf8(r$norm_name)
  r$norm_name <-stringr::str_replace_all(r$norm_name,"[^\\p{Latin}\\p{Greek}\\p{Cyrillic}0-9]"," ")
  rt<-r[iso_country(r[,country])%in%c("BG","CY","GR")&stringr::str_detect(r[,name],"[\\p{Greek}\\p{Cyrillic}]"),]
  r<-r[!r[,key]%in%rt[,key],]
  if(translit==T&nrow(rt)>0){
    for (t in unique(rt[,country]))
    {
     rtc<-norma_trans(rt[rt[,country]==t,],"norm_name",country,key)
     r<-rbind(r,rtc)
    }
  }
  r$norm_name <-stringi::stri_trans_general(r$norm_name, "latin-ascii; upper")
  #r$norm_name <-stri_replace_all_regex(r$norm_name,"\\([[:alnum:]]{2})>", "\\\\u00$1")
  #r$norm_name <-stringi::stri_unescape_unicode(r$norm_name)
  r$norm_name <-stringr::str_replace_all(r$norm_name,"W UPADLOSCI (LIKWIDACYJNEJ)?|W LIKWIDACJI","")
  r$norm_name <-stringr::str_replace_all(r$norm_name,"CREDIT NEEDED|DOUBLE DO ?NOT.*|(DOUBLE)? ?(PLE?A?S?E?)? ?(USE?|WITH|OF) ID ?W? ?(NO)? ?[1-9]+.*|DOUBLE ?(PLE?A?S?E?)? ?(USE?|WITH|OF).*|NOT VALID.*|PLEASE ?USE ?W? ?[1-9]+.*|(PLE?A?S?E?)? DO ?NOT USE.*|USE NOW ID*|PLS USE.*|DOUBLE.*USE.*[0-9]+?.*|DOUBLE$","")
  r$norm_name <-stringr::str_replace_all(r$norm_name,"\\n"," ")
  r$norm_name <-stringr::str_replace_all(r$norm_name,"\\t"," ")
  r$norm_name <-stringr::str_replace_all(r$norm_name,"\\s+"," ")
  r$norm_name<-stringr::str_replace_all(r$norm_name,"\\\\","")
  r$norm_name <-stringr::str_trim(r$norm_name)



  if(short==T){
    r<-norma_short(r,"norm_name")
  }


  if(legal==T){
    r<-norma_legal(r,"norm_name",country)
    r$lf<-stringr::str_replace(r$lf,";\\s+?$","")
    r<-tidyr::separate_rows(r,lf,sep=";")
    r$lf<-stringr::str_trim(r$lf)
    r<-r[order(r$lf),]
    rlf<-unique(r[,c(key,"lf")])
    rlf<-data.table::data.table(rlf)[,list(lf=paste0(lf,collapse=";")),
                         by=key]
    r$lf<-NULL
    r<-merge(r,rlf,by=key)
    r$lf<-stringr::str_replace_all(r$lf,"^;|;$","")
    r<-unique(r)
    # r<-merge(r,rlf,by=key)
  }

  r$norm_name<-stringr::str_replace_all(r$norm_name,"( |^)GROUP( |$)|HOLDING|CORPORATION|INCORPORATED|\\bCO\\b|LIMITED|\\bLTD\\b| ALSO$","")

  ###The last step of the process consists of correcting those cases where the process of normalization left normalized name as an empty string

  re<-r[r$norm_name=="",]

  if(nrow(re)!=0)
  {
    re$norm_name <-stringi::stri_trans_general(re[,name], "latin-ascii; upper")
    re$norm_name <-stringi::stri_enc_toutf8(re$norm_name)
    re$norm_name <-stringr::str_replace_all(re$norm_name,"[^\\p{Latin}\\p{Greek}\\p{Cyrillic}0-9]"," ")
    re$norm_name <-stringr::str_replace_all(re$norm_name,"W UPADLOSCI (LIKWIDACYJNEJ)?|W LIKWIDACJI","")
    re$norm_name <-stringr::str_replace_all(re$norm_name,"CREDIT NEEDED|DOUBLE DO ?NOT.*|(DOUBLE)? ?(PLE?A?S?E?)? ?(USE?|WITH|OF) ID ?W? ?(NO)? ?[1-9]+.*|DOUBLE ?(PLE?A?S?E?)? ?(USE?|WITH|OF).*|NOT VALID.*|PLEASE ?USE ?W? ?[1-9]+.*|(PLE?A?S?E?)? DO ?NOT USE.*|USE NOW ID*|PLS USE.*|DOUBLE.*USE.*[0-9]+?.*|DOUBLE$","")
    re$norm_name <-stringr::str_replace_all(re$norm_name,"\\n"," ")
    re$norm_name <-stringr::str_replace_all(re$norm_name,"\\t"," ")
    re$norm_name <-stringr::str_replace_all(re$norm_name,"\\s+"," ")
    re$norm_name <-stringr::str_trim(re$norm_name)
    rf<-r[r$norm_name!="",]
    r<-plyr::rbind.fill(re,rf)
  }

  if(new_col==F){
    r[,name]<-r$norm_name
    r$norm_name<-NULL
  }



  return(r)
}

