#' Build a lookup table from the city patterns of one country
#'
#' Every pattern in `cities` has the form "(^| )NAME( |$)": NAME must appear as a run of whole words.
#' The table keeps NAME as a plain key, with the row of `citiesc` it came from.
#' @param citiesc rows of `cities` for one country, in their original order
#' @returns a list with the keys, their rows in `citiesc` and the largest number of words in a key
#' @noRd

city_lookup<-function(citiesc){
  if(nrow(citiesc)==0) stop("no cities found for this country code")
  pat<-as.character(citiesc$Alternate.Names)
  key<-substr(pat,6,nchar(pat)-5)
  row<-seq_along(key)
  ###a few keys end in a regex class such as "[A ]" or "[IPA]": write one key per letter of the class
  m<-regexpr("\\[[^]]*\\]$",key)
  if(any(m>0)){
    cls<-which(m>0)
    alt<-lapply(stringr::str_extract_all(regmatches(key,m),"[A-Z]"),unique)
    key<-c(key[-cls],unlist(Map(paste0,substr(key[cls],1,m[cls]-1),alt)))
    row<-c(row[-cls],rep(row[cls],lengths(alt)))
  }
  o<-order(row)
  key<-key[o]
  row<-row[o]
  ###the first row wins, as it did in the loop over the patterns
  keep<-!duplicated(key)
  list(key=key[keep],row=row[keep],max_words=max(lengths(strsplit(key," ",fixed=TRUE))))
}

#' Find the first matching city pattern for each string
#'
#' Gives the same result as testing the patterns of `citiesc` one by one, in order, and keeping the first
#' that matches. Each string is split into words and every run of consecutive words (an n-gram) is looked up
#' in the table. Of all hits, the one with the lowest row in `citiesc` wins.
#' @param u distinct strings to look up, without NA
#' @param lk the table returned by city_lookup()
#' @returns for each string, the row of the matching city in `citiesc`, or NA when nothing matches
#' @noRd

city_match<-function(u,lk){
  w<-strsplit(u," ",fixed=TRUE)
  k<-lengths(w)
  id<-rep(seq_along(u),k)
  pos<-sequence(k)
  word<-unlist(w,use.names=FALSE)
  last<-k[id]
  gram<-word
  hit_id<-integer(0)
  hit_row<-integer(0)
  for(n in seq_len(lk$max_words)){
    ###extend each n-gram by the next word; keep only the n-grams that stay inside their string
    if(n>1) gram<-paste(gram,c(word[-seq_len(n-1)],rep("",n-1)))
    ok<-pos+n-1<=last
    if(!any(ok)) break
    h<-match(gram[ok],lk$key)
    f<-!is.na(h)
    hit_id<-c(hit_id,id[ok][f])
    hit_row<-c(hit_row,lk$row[h[f]])
  }
  o<-order(hit_id,hit_row)
  first<-o[!duplicated(hit_id[o])]
  out<-rep(NA_integer_,length(u))
  out[hit_id[first]]<-hit_row[first]
  out
}
