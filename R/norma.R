#' @import data.table
#' @import stringr
#' @import stringi
#' @import dplyr
#' @import tidyr

#' @title returns an original dataset with normalized name
#'
#' @description This is the main function of the package
#' @param r dataset to be transformed
#' @param name name of the column storing name to be transformed, it has to be a string
#' @param country name of the column storing name of the country of seat
#' @param key name of the column storing unique identifier of the entity
#' @param new_col if TRUE creates new column with normalized version of the name along with the original version of the name
#' @param short if TRUE looks for 'trading as' expressions with the name
#' @param translit if TRUE looks for names written in cyrylic and transforms them into their latin versions
#' @param legal if TRUE looks for legal form expressions in the name and deals with them
#' @returns an original dataframe with two additional copies of records containing 'trading as' expression
#' @examples df<-data.frame(name="Andrzej Beata Celina spółka z ograniczoną odpowiedzialnoscią nazwa skrócona ABC sp. z o.o. SP.J",
#'  country="PL",key=1)
#' @export

#dfn<-norma(df,"name","country","key",new_col=T,short=T,translit=T,legal=T)


norma<-function(r,name,country,key,new_col=T,short=T,translit=T,legal=T){
  ###This is to account for the choice of whether to create a new column for transliterated names
  ##or to transform the column which stored the original name
  r$norm_name<-r[,name]
  if(translit==T&unique(r[,country])%in%c("BG","CY","GR")){
    r<-norma_trans(r,"norm_name",country,key)
  }
  r$norm_name <-stri_trans_general(r$norm_name, "latin-ascii; upper")
  r$norm_name <-str_replace_all(r$norm_name,"\\.","")
  r$norm_name <-str_replace_all(r$norm_name,"-"," ")
  #r$norm_name <-stri_replace_all_regex(r$norm_name,"\\([[:alnum:]]{2})>", "\\\\u00$1")
  r$norm_name <-stri_unescape_unicode(r$norm_name)
  r$norm_name <-stri_enc_toutf8(r$norm_name)
  r$norm_name <-str_replace_all(r$norm_name,"[^a-zA-Z0-9]"," ")
  r$norm_name <-str_replace_all(r$norm_name,"\\n"," ")
  r$norm_name <-str_replace_all(r$norm_name,"\\t"," ")
  r$norm_name <-str_replace_all(r$norm_name,"\\s+"," ")
  r$norm_name <-str_trim(r$norm_name)


  if(short==T){
    r<-norma_short(r,"norm_name")
  }


  if(legal==T){
    r<-norma_legal(r,"norm_name",country)
    r$lf<-str_replace(r$lf,";\\s+?$","")
    r<-separate_rows(r,lf,sep=";")
    r$lf<-str_trim(r$lf)
    r<-r[order(r$lf),]
    rlf<-unique(r[,c(key,"lf")])
    rlf<-data.table(rlf)[,.(lf=paste0(lf,collapse=";")),
                         by=key]
    r$lf<-NULL
    r<-merge(r,rlf,by=key)
    r$lf<-str_replace_all(r$lf,"^;|;$","")
    r<-unique(r)
    # r<-merge(r,rlf,by=key)
  }

  r$norm_name<-str_replace_all(r$norm_name,"GROUP|HOLDING|CORPORATION|INCORPORATED|\bCO\b|LIMITED|\bLTD\b","")

  if(new_col==F){
    r[,name]<-r$norm_name
    r$norm_name<-NULL
  }


  return(r)
}

