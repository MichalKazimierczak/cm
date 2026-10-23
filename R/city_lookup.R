#' Build a lookup table from the city patterns of one country
#'
#' Every pattern in `cities` has the form "(^| )NAME( |$)": NAME must appear as a run of whole words.
#' The table keeps NAME as a plain key, with the row of `citiesc` it came from. Each row gives at most one
#' key.
#' @param citiesc rows of `cities` for one country, in their original order
#' @returns a list with the keys, their rows in `citiesc` and the largest number of words in a key
#' @noRd

city_lookup<-function(citiesc){
  if(nrow(citiesc)==0) stop("no cities found for this country code")
  pat<-as.character(citiesc$Alternate.Names)
  key<-substr(pat,6,nchar(pat)-5)
  row<-seq_along(key)
  ###GeoNames marks some historical names with a note such as "[a. 704]" and phonetic transcriptions with
  ###"[IPA]". data-raw/cities.R already removes them; this keeps lists built another way safe ("[A ]").
  ipa<-grepl("[IPA]",key,fixed=TRUE)
  key<-sub(" ?\\[[^]]*\\]$","",key)[!ipa]
  row<-row[!ipa]
  ###the first row wins, as it did in the loop over the patterns
  keep<-!duplicated(key)
  list(key=key[keep],row=row[keep],max_words=max(lengths(strsplit(key," ",fixed=TRUE))))
}

#' Find all city names in each string, with their positions
#'
#' Each string is split into words and every run of consecutive words (an n-gram) is looked up in the table.
#' @param u distinct strings to look up, without NA
#' @param lk the table returned by city_lookup()
#' @returns a data frame with one row per hit: the string (\code{id}), the row of the city in \code{citiesc}
#'   (\code{row}) and the positions of its first and last word in the string (\code{from}, \code{to})
#' @noRd

city_hits<-function(u,lk){
  w<-strsplit(u," ",fixed=TRUE)
  k<-lengths(w)
  id<-rep(seq_along(u),k)
  pos<-sequence(k)
  word<-unlist(w,use.names=FALSE)
  last<-k[id]
  gram<-word
  hit_id<-integer(0)
  hit_row<-integer(0)
  hit_from<-integer(0)
  hit_n<-integer(0)
  for(n in seq_len(lk$max_words)){
    ###add the word that follows each n-gram; keep only the n-grams that stay inside their string
    if(n>1) gram<-paste(gram,c(word[-seq_len(n-1)],rep("",n-1)))
    ok<-pos+n-1<=last
    if(!any(ok)) break
    h<-match(gram[ok],lk$key)
    f<-!is.na(h)
    hit_id<-c(hit_id,id[ok][f])
    hit_row<-c(hit_row,lk$row[h[f]])
    hit_from<-c(hit_from,pos[ok][f])
    hit_n<-c(hit_n,rep(n,sum(f)))
  }
  data.frame(id=hit_id,row=hit_row,from=hit_from,to=hit_from+hit_n-1L)
}

#' Find the first matching city pattern for each string
#'
#' Gives the same result as testing the patterns of `citiesc` one by one, in order, and keeping the first
#' that matches (with the notes removed by city_lookup()). Of all hits found by city_hits(), the one with the
#' lowest row in `citiesc` wins.
#' @param u distinct strings to look up, without NA
#' @param lk the table returned by city_lookup()
#' @returns for each string, the row of the matching city in `citiesc`, or NA when nothing matches
#' @noRd

city_match<-function(u,lk){
  h<-city_hits(u,lk)
  o<-order(h$id,h$row)
  first<-o[!duplicated(h$id[o])]
  out<-rep(NA_integer_,length(u))
  out[h$id[first]]<-h$row[first]
  out
}
