#' @title Build a table of first names from data with separate first-name fields
#'
#' @description Counts how often each word occurs in first names of natural persons, per country, and how often
#' it occurs in names of organisations. The table lets \code{\link{guess_person}} and \code{\link{match_quality}}
#' recognise persons in sources without a type field (for example PATSTAT records without \code{psn_sector}).
#' Build it once, for example from the first-name field of natural persons in the EUIPO register, and keep it with
#' your data (\code{saveRDS()}): it is derived from personal data and is not part of the package.
#' @param first_names first names of natural persons (one or more per value, such as "JAN PIETER")
#' @param country country of each person (ISO 3166-1 code), or \code{NULL} for one table for all countries
#' @param org_names optional: names of organisations, to measure how often a first name occurs in them (MARK,
#' ROSE and VICTORIA often do); without them every first name counts as a person word
#' @param org_country country of each organisation, or \code{NULL}
#' @param min_count words occurring fewer times than this as a first name are left out (typing errors, initials)
#' @returns a data frame with one row per word and country, and per word for all countries together
#' (\code{country = "*"}): \code{word}, \code{country}, \code{n_person} (persons with this word in their first
#' name), \code{n_org} (organisation names containing the word) and \code{person_share}: the rate among the
#' country's persons divided by the sum of the rates among its persons and its organisations, so that both groups
#' weigh equally (1 means never seen in organisation names)
#' @examples
#' fn <- build_first_names(c("Jan", "Jan Pieter", "Maria", "Mark", "Mark"), country = c("NL", "NL", "IT", "DE", "DE"),
#'                         org_names = c("Mark Media GmbH", "Pieter Bouw BV"), org_country = c("DE", "NL"), min_count = 1)
#' fn[fn$country == "*", ]
#' @export

build_first_names<-function(first_names,country=NULL,org_names=NULL,org_country=NULL,min_count=5){
  ###one row per word of each first name, counted once per person
  words<-function(x,cc){
    w<-strsplit(clean_person_name(x)," ",fixed=TRUE)
    d<-data.frame(id=rep(seq_along(w),lengths(w)),word=unlist(w,use.names=FALSE),
                  country=rep(if(is.null(cc)) rep("*",length(x)) else iso_country(cc),lengths(w)),stringsAsFactors=FALSE)
    d<-d[nchar(d$word)>=2&!is.na(d$word),]
    unique(d)
  }
  p<-words(first_names,country)
  o<-if(is.null(org_names)) p[0,] else words(org_names,org_country)
  ###counts per word and country, and per word for all countries ("*")
  count<-function(d) {
    by_c<-if(nrow(d)) stats::aggregate(id~word+country,data=d,FUN=length) else data.frame(word=character(0),country=character(0),id=integer(0))
    all<-if(nrow(d)) stats::aggregate(id~word,data=unique(d[,c("id","word")]),FUN=length) else data.frame(word=character(0),id=integer(0))
    all$country<-rep("*",nrow(all))
    rbind(by_c[by_c$country!="*",c("word","country","id")],all[,c("word","country","id")])
  }
  ###number of persons and of organisations per country, and over all countries
  totals<-function(cc,n) {
    t<-table(if(is.null(cc)) rep("*",n) else iso_country(cc))
    c(stats::setNames(as.vector(t),names(t)),"*"=n)
  }
  n_p<-totals(country,length(first_names))
  n_o<-if(is.null(org_names)) NULL else totals(org_country,length(org_names))
  cp<-count(p)
  names(cp)[3]<-"n_person"
  co<-count(o)
  names(co)[3]<-"n_org"
  t<-merge(cp,co,by=c("word","country"),all.x=TRUE)
  t$n_org[is.na(t$n_org)]<-0L
  t<-t[t$n_person>=min_count,]
  ###rates among persons and organisations of the same country (or of all countries for "*"), weighted equally,
  ###so that the size of the two groups does not matter
  rp<-t$n_person/n_p[t$country]
  ro<-if(is.null(org_names)) 0 else ifelse(is.na(n_o[t$country]),0,t$n_org/n_o[t$country])
  t$person_share<-rp/(rp+ro)
  t<-t[order(t$country,-t$n_person,t$word),]
  rownames(t)<-NULL
  t
}

#' @title Guess whether names are names of natural persons
#'
#' @description Looks for a first name from a table built by \code{\link{build_first_names}}. Names written as
#' "SURNAME, FIRSTNAME" (as in PATSTAT) are looked up after the comma; names without a comma at their first and last
#' word. Names with a legal form or a company word are organisations.
#' @param names names to classify
#' @param country country of each name (ISO 3166-1; Eurostat's EL and UK work too), or \code{NULL}; a word is looked
#' up for the name's country when that country has at least \code{min_count} persons with it (its share there
#' decides, even when low), otherwise for all countries
#' @param first_names the table from \code{\link{build_first_names}}
#' @param min_share words with a smaller \code{person_share} do not count as first names
#' @param min_count words with a smaller \code{n_person} do not count as first names
#' @returns a data frame with one row per name: \code{type} ("person", "organisation" or \code{NA} when the names
#' give no sign), \code{first_name} (the first name found) and \code{person_share} (its share in the table)
#' @examples
#' fn <- build_first_names(c("Jan", "Jan Pieter", "Maria"), country = c("NL", "NL", "IT"), min_count = 1)
#' guess_person(c("DE VRIES, JAN", "Maria Rossi", "Alpha Beta", "Rossi Media Srl"), country = c("NL", "IT", "IT", "IT"),
#'              first_names = fn)
#' @export

guess_person<-function(names,country=NULL,first_names,min_share=0.9,min_count=5){
  n0<-length(names)
  raw0<-as.character(names)
  cc0<-if(is.null(country)) rep("*",n0) else iso_country(country)
  cc0[is.na(cc0)]<-"*"
  ###each distinct name and country once
  key<-paste(raw0,cc0,sep="\r")
  uk<-!duplicated(key)
  raw<-raw0[uk]
  cc<-cc0[uk]
  n<-length(raw)
  ###words seen often enough; the share is applied after choosing the row, so that a word that is rarely a first
  ###name in the name's country is not taken from the table of all countries
  fn<-first_names[first_names$n_person>=min_count,]
  ###candidate first-name words: after the comma ("SURNAME, FIRSTNAME"), otherwise the first and the last word
  ###other scripts to Latin letters once (the slow step); the comma survives it
  latin<-to_latin(raw)
  comma<-grepl(",",latin,fixed=TRUE)
  w<-strsplit(squish_words(ifelse(comma,sub("^[^,]*,","",latin),latin))," ",fixed=TRUE)
  k<-lengths(w)
  id<-rep(seq_len(n),k)
  pos<-sequence(k)
  keep<-comma[id]|k[id]<=2|pos==1|pos==k[id]
  d<-data.frame(i=id[keep],word=unlist(w,use.names=FALSE)[keep],stringsAsFactors=FALSE)
  d$cc<-cc[d$i]
  ###the name's country first, then all countries
  k_c<-match(paste(d$word,d$cc),paste(fn$word,fn$country))
  k_a<-match(paste(d$word,"*"),paste(fn$word,fn$country))
  kk<-ifelse(is.na(k_c),k_a,k_c)
  d$share<-fn$person_share[kk]
  d<-d[!is.na(d$share)&d$share>=min_share,]
  d<-d[order(d$i,-d$share),]
  d<-d[!duplicated(d$i),]
  out<-data.frame(type=rep(NA_character_,n),first_name=rep(NA_character_,n),person_share=rep(NA_real_,n),stringsAsFactors=FALSE)
  out$type[d$i]<-"person"
  out$first_name[d$i]<-d$word
  out$person_share[d$i]<-d$share
  ###a legal form or a company word makes an organisation, whatever the first names say
  org<-!is.na(raw)&stringi::stri_detect_regex(squish_words(latin),company_words)
  out$type[org]<-"organisation"
  out<-out[match(key,key[uk]),]
  rownames(out)<-NULL
  out
}

## Names as the first-name functions compare them: other scripts in Latin letters, ASCII capitals, words separated by
## one space (punctuation turned into spaces)
clean_person_name<-function(x) squish_words(to_latin(x))
to_latin<-function(x) stringi::stri_trans_general(ifelse(is.na(x),"",as.character(x)),"Any-Latin; Latin-ASCII; Upper")
squish_words<-function(x) stringr::str_squish(stringr::str_replace_all(x,"[^A-Z0-9]"," "))
