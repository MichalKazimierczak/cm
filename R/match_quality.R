#' @title Flag the quality of matched pairs
#'
#' @description Describes how well each matched pair is supported by the data, so that users can decide which
#' matches to keep. Nothing is removed or changed: the function adds columns whose names start with \code{q_}.
#' Use it on the output of \code{\link{clean}} or \code{\link{rankmatch}}. Optional inputs (the stored output of
#' \code{\link{norma}} for each dataset, and columns with the entity type) add more flags; without them, those
#' flags are \code{NA}.
#' @param pairs matched pairs: the output of \code{\link{clean}} or \code{\link{rankmatch}}
#' @param key_a,key_b names of the columns with the identifiers of datasets A and B
#' @param name_a,name_b names of the columns with the original names (as given to \code{clean()})
#' @param lf_a,lf_b names of the columns with the legal forms (several separated by \code{;}), or \code{NULL}
#' @param zip_a,zip_b,city_a,city_b,street_a,street_b names of the address columns, or \code{NULL} when not available
#' @param match_key name of the column with the key \code{\link{nmatch}} matched on
#' @param norm_a,norm_b optional: the stored output of \code{\link{norma}} for dataset A and B, used to count how
#' many entities share a name and to find matches made through part of a name ('trading as')
#' @param norm_key_a,norm_key_b identifier columns in \code{norm_a} and \code{norm_b}
#' @param norm_name_a,norm_name_b columns in \code{norm_a} and \code{norm_b} with the names given to \code{nmatch()}
#' @param sort,unique the settings used in \code{\link{nmatch}}, so that the keys are built the same way
#' @param type_a,type_b optional: names of columns in \code{pairs} with the entity type, for example the EUIPO
#' applicant type and PATSTAT \code{psn_sector}
#' @param type_side which side decides the type of the pair: \code{"both"} (both sides, "mixed" when they differ),
#' or only \code{"a"} or \code{"b"}, for example the IP side when the other side is Orbis
#' @param first_names optional: a table from \code{\link{build_first_names}}; names without a type and without a
#' legal form are persons when they contain one of its first names (see \code{\link{guess_person}})
#' @param country_a,country_b optional: names of columns with the countries, to look first names up per country
#' @param first_name_share,first_name_count passed to \code{\link{guess_person}} as \code{min_share} and
#' \code{min_count}
#' @param person_like_share a name is person-like when it contains a word that is a first name at least this share
#' of the time in \code{first_names}; lower than \code{first_name_share}, because a risk flag may err on the cautious
#' side
#' @param reference the side whose names are used to judge how distinctive a name is, normally the larger reference
#' database (Orbis): \code{"b"} or \code{"a"}; needs \code{norm_b} (or \code{norm_a})
#' @param namesakes_max a name is not distinctive when the expected number of reference names with all its words
#' (\code{q_namesakes}) is at least this
#' @param name_sim_min original names (words sorted) less similar than this differ
#' @param city_sim_min,street_sim_min cities (streets) at least this similar agree; cities below it are a conflict
#' @param margin_min a best candidate whose score is less than this above the next candidate is a close call
#' @param short_key matching keys with fewer characters than this are short
#' @param person_pattern,org_pattern regular expressions (on the type values in capitals) for persons and for
#' organisations; organisations are tested first, so "LEGAL PERSON" is an organisation
#' @details Classes (\code{q_class}):
#' \describe{
#'   \item{A, confirmed}{no conflict and at least one agreeing piece of evidence besides the name}
#'   \item{B, plausible}{no conflict, matched on the name alone, with a name that is not weak}
#'   \item{C, uncertain}{no conflict, but the pair is not the best candidate of its entity, a close call, or matched
#'   on the name alone with a weak name: shared by several entities, short, matched through a 'trading as' part,
#'   person against organisation, original names that differ, person-like, or not distinctive}
#'   \item{D, conflicting}{at least one conflict: legal forms with nothing in common, different regions, or different
#'   cities}
#' }
#' The entity type is taken from \code{type_a} and \code{type_b} where they say person or organisation; otherwise,
#' with \code{first_names}, from a first name in the name (\code{q_type_source} "first name"); otherwise it is
#' guessed: a person has no legal form and a name of two to four words without digits, company words or
#' legal-form words. The original names are compared with their words sorted (\code{q_name_sim}), so that a surname
#' first equals a surname last; differing names are only a weak signal, because normalisation removes words on
#' purpose. Without \code{norm_a} and \code{norm_b}, matches through part of a name cannot be recognised and their
#' names show as differing.
#' The person-like flag and distinctiveness are risk signals, not classifications: a company named after a person
#' (JAN DE VRIES HOLDING) is as easy to confuse as the person. Distinctiveness uses the words of the matched name on
#' the \code{reference} side: \code{q_rarest_word} is the number of reference names with its rarest word, and
#' \code{q_namesakes} the number of reference names expected to contain all its words by chance (the number of
#' names times the share of names with each word, multiplied); 1 or more means other entities with the name are
#' likely.
#' Matches through part of a name are recognised when another name of the same entity in \code{norm_a} or
#' \code{norm_b} contains the matched name as whole words (\code{JAN DE VRIES} in
#' \code{JAN DE VRIES H O D N BAKKERIJ DE MOLEN}).
#' @returns \code{pairs} as a data frame, rows unchanged, with the columns \code{q_class}, \code{q_reasons} (why, in
#' words), \code{q_evidence} (signals present on both sides: legal form, region, city, street), \code{q_agree}
#' (signals that agree), \code{q_conflict_lf}, \code{q_conflict_region}, \code{q_conflict_city} (\code{NA} when a side
#' has no value), \code{q_name_sim} (similarity of the original names, words sorted), \code{q_names_differ},
#' \code{q_candidates_a}, \code{q_candidates_b} (number of candidates of the
#' A and the B entity), \code{q_best} (best candidate on both sides), \code{q_margin} (score gap to the next
#' candidate), \code{q_key_freq_a}, \code{q_key_freq_b} (entities sharing the name), \code{q_alias}, \code{q_type_a},
#' \code{q_type_b}, \code{q_type_source_a}, \code{q_type_source_b} (field, first name or guessed) and
#' \code{q_type_pair} (organisation, person or mixed), \code{q_person_like}, \code{q_namesakes} and
#' \code{q_rarest_word}
#' @examples
#' pairs <- data.frame(id_a = c(1, 2, 3, 3), id_b = c(11, 12, 13, 14), norm_name = c("ALPHA", "DEJANVRIES", "BETA", "BETA"),
#'                     name_a = c("Alpha GmbH", "Jan de Vries", "Beta AG", "Beta AG"),
#'                     name_b = c("ALPHA GMBH", "JAN DE VRIES", "BETA AG", "BETA AKTIENGESELLSCHAFT"),
#'                     lf_a = c("GMBH", "", "AG", "AG"), lf_b = c("GMBH", "", "AG", "AG"),
#'                     city_a = c("MUNICH", NA, "BERLIN", "BERLIN"), city_b = c("MUNICH", NA, "BERLIN", "COLOGNE"),
#'                     one_to_one = TRUE, name_sim = c(1, 1, 1, 0.77), city_sim = c(1, 0, 1, 0.5), street_sim = 0,
#'                     reg_a = c("DE212", NA, "DE300", "DE300"), reg_b = c("DE212", NA, "DE300", "DEA23"),
#'                     reg_sim = c(TRUE, FALSE, TRUE, FALSE), final_score = c(0.8, 0.4, 0.8, 0.49))
#' q <- match_quality(pairs, "id_a", "id_b", "name_a", "name_b", lf_a = "lf_a", lf_b = "lf_b",
#'                    city_a = "city_a", city_b = "city_b")
#' q[, c("id_a", "id_b", "q_class", "q_reasons")]
#' @export

match_quality<-function(pairs,key_a,key_b,name_a,name_b,lf_a=NULL,lf_b=NULL,zip_a=NULL,zip_b=NULL,
                        city_a=NULL,city_b=NULL,street_a=NULL,street_b=NULL,match_key="norm_name",
                        norm_a=NULL,norm_b=NULL,norm_key_a=key_a,norm_key_b=key_b,
                        norm_name_a="norm_name",norm_name_b="norm_name",sort=TRUE,unique=TRUE,
                        type_a=NULL,type_b=NULL,type_side=c("both","a","b"),first_names=NULL,country_a=NULL,
                        country_b=NULL,first_name_share=0.9,first_name_count=5,person_like_share=0.5,
                        reference=c("b","a"),namesakes_max=1,
                        name_sim_min=0.8,city_sim_min=0.8,street_sim_min=0.8,
                        margin_min=0.05,short_key=5,
                        person_pattern="INDIVIDUAL|NATURAL|PHYSICAL|PERSON",
                        org_pattern="COMPANY|LEGAL|ENTITY|GOV|UNIVERSIT|HOSPITAL|ORGANI|NON-PROFIT"){
  r<-as.data.frame(pairs)
  type_side<-match.arg(type_side)
  reference<-match.arg(reference)
  need<-c(key_a,key_b,name_a,name_b,match_key,"one_to_one","name_sim","city_sim","street_sim","reg_a","reg_b","reg_sim")
  miss<-setdiff(need,names(r))
  if(length(miss)>0) stop("match_quality: pairs lacks the columns ",paste(miss,collapse=", "),
                          " (use the output of clean() or rankmatch())")
  n<-nrow(r)
  ###a column as text, or NA when the column is not given
  col<-function(x) if(is.null(x)) rep(NA_character_,n) else as.character(r[[x]])
  has<-function(x) !is.na(x)&stringr::str_trim(x)!=""
  ###values separated by sep, as a list of sets ("GMBH;KG", "DE212, DE300")
  ###do two lists of values ("GMBH;KG" and "GMBH", "DE212, DE300" and "DE212") have a value in common, row by row
  overlap<-function(x,y,sep){
    long<-function(v){
      w<-strsplit(as.character(ifelse(is.na(v),"",as.character(v))),sep,fixed=TRUE)
      d<-data.table::data.table(i=rep(seq_along(w),lengths(w)),v=stringi::stri_trim_both(unlist(w,use.names=FALSE)))
      unique(d[d$v!="",])
    }
    m<-merge(long(x),long(y),by=c("i","v"))
    seq_len(n)%in%m$i
  }

  ###1. evidence: the signals present on both sides, and those that agree
  lfa<-col(lf_a)
  lfb<-col(lf_b)
  ev_lf<-has(lfa)&has(lfb)
  ev_reg<-(has(r$reg_a)&has(r$reg_b))|(has(col(zip_a))&has(col(zip_b)))
  ev_city<-has(col(city_a))&has(col(city_b))
  ev_street<-has(col(street_a))&has(col(street_b))
  lf_common<-overlap(lfa,lfb,";")
  reg_common<-r$reg_sim%in%TRUE|overlap(r$reg_a,r$reg_b,",")
  ag_lf<-ev_lf&lf_common
  ag_reg<-ev_reg&reg_common
  ag_city<-ev_city&r$city_sim>=city_sim_min
  ag_street<-ev_street&r$street_sim>=street_sim_min
  q_evidence<-as.integer(ev_lf+ev_reg+ev_city+ev_street)
  q_agree<-as.integer(ag_lf+ag_reg+ag_city+ag_street)

  ###2. conflicts, only where both sides have a value
  both_reg<-has(r$reg_a)&has(r$reg_b)
  q_conflict_lf<-ifelse(ev_lf,!lf_common,NA)
  q_conflict_region<-ifelse(both_reg,!reg_common,NA)
  q_conflict_city<-ifelse(ev_city,r$city_sim<city_sim_min,NA)
  conflict<-q_conflict_lf%in%TRUE|q_conflict_region%in%TRUE|q_conflict_city%in%TRUE

  ###3. ambiguity: candidates per entity, best candidate and score gap to the next one
  ka<-as.character(r[[key_a]])
  kb<-as.character(r[[key_b]])
  score<-if("final_score"%in%names(r)) r$final_score else rep(NA_real_,n)
  side<-function(g,o){
    d<-data.table::data.table(row=seq_len(n),g=g,o=o,s=score)
    cnt<-d[!is.na(g),list(cand=data.table::uniqueN(o)),by=g]
    cand<-cnt$cand[match(g,cnt$g)]
    best<-rep(NA,n)
    margin<-rep(NA_real_,n)
    ###best score of each candidate of an entity with several (a candidate can appear in several rows)
    sel<-!is.na(g)&!is.na(score)&g%in%cnt$g[cnt$cand>1]
    if(any(sel)){
      bo<-d[sel,list(s=max(s)),by=list(g,o)]
      bo<-bo[order(bo$g,-bo$s,bo$o),]
      bo$rank<-stats::ave(seq_len(nrow(bo)),bo$g,FUN=seq_along)
      top<-bo[bo$rank==1,]
      sec<-bo[bo$rank==2,]
      m<-match(g,top$g)
      has<-!is.na(m)&!is.na(score)
      best[has]<-o[has]==top$o[m[has]]&score[has]>=top$s[m[has]]
      margin[has]<-top$s[m[has]]-sec$s[match(g[has],sec$g)]
    }
    best[!is.na(cand)&cand==1]<-TRUE
    list(cand=as.integer(cand),best=best,margin=margin)
  }
  sa<-side(ka,kb)
  sb<-side(kb,ka)
  q_candidates_a<-sa$cand
  q_candidates_b<-sb$cand
  q_best<-sa$best&sb$best
  q_margin<-pmin(sa$margin,sb$margin,na.rm=TRUE)
  ambiguous<-(q_candidates_a>1)%in%TRUE|(q_candidates_b>1)%in%TRUE|!(r$one_to_one%in%TRUE)

  ###4. names: how many entities share the name, short names, matches through part of a name
  mk<-as.character(r[[match_key]])
  ###the stored names of each side once: distinct entity and name, with the key nmatch() builds from the name
  prep<-function(nt,idcol,namecol){
    if(is.null(nt)) return(NULL)
    nt<-as.data.frame(nt)
    d<-unique(data.frame(id=as.character(nt[[idcol]]),nm=as.character(nt[[namecol]]),stringsAsFactors=FALSE))
    d<-d[!is.na(d$nm)&d$nm!="",]
    d$k<-match_keys(d$nm,sort,unique)
    d
  }
  pa<-prep(norm_a,norm_key_a,norm_name_a)
  pb<-prep(norm_b,norm_key_b,norm_name_b)
  freq<-function(d){
    if(is.null(d)) return(rep(NA_integer_,n))
    u<-unique(d[!is.na(d$k),c("id","k")])
    cnt<-table(u$k)
    as.integer(cnt[match(mk,names(cnt))])
  }
  q_key_freq_a<-freq(pa)
  q_key_freq_b<-freq(pb)
  alias<-function(d,pid){
    if(is.null(d)) return(rep(NA,n))
    ###a name is a part when another name of the same entity contains it as whole words
    multi<-d[d$id%in%d$id[duplicated(d$id)],]
    j<-merge(multi,multi,by="id",suffixes=c("","_long"))
    j<-j[j$nm!=j$nm_long&stringi::stri_detect_fixed(paste0(" ",j$nm_long," "),paste0(" ",j$nm," ")),]
    out<-paste(pid,mk)%in%paste(j$id,j$k)
    out[!pid%in%d$id]<-NA
    out
  }
  al_a<-alias(pa,ka)
  al_b<-alias(pb,kb)
  q_alias<-ifelse(al_a%in%TRUE|al_b%in%TRUE,TRUE,ifelse(is.na(al_a)&is.na(al_b),NA,FALSE))
  shared<-(q_key_freq_a>1)%in%TRUE|(q_key_freq_b>1)%in%TRUE
  ###similarity of the original names with their words sorted, so that DE VRIES JAN equals JAN DE VRIES;
  ###not checked for matches through part of a name, where the full name differs by nature
  words_sorted<-function(x){
    x<-stringi::stri_trans_general(stringr::str_replace_all(as.character(ifelse(is.na(x),"",x)),"[^\\p{Latin}\\p{Greek}\\p{Cyrillic}0-9]"," "),"latin-ascii; upper")
    sort_words(stringr::str_squish(x))
  }
  q_name_sim<-stringdist::stringsim(words_sorted(col(name_a)),words_sorted(col(name_b)),method="jw")
  q_names_differ<-ifelse(q_alias%in%TRUE,NA,q_name_sim<name_sim_min)
  ###distinctiveness, on the reference side: how many of its names contain each word of the matched name. The
  ###expected number of names with all these words by chance is N * (share of names with word 1) * (share with
  ###word 2) * ...; 1 or more means that other entities with this name are likely (JAN DE VRIES)
  distinct<-function(d,pid){
    if(is.null(d)) return(list(ns=rep(NA_real_,n),rare=rep(NA_integer_,n)))
    nms<-base::unique(d$nm)
    N<-length(nms)
    ###each word once per name, then the number of names with each word
    w<-strsplit(nms," ",fixed=TRUE)
    wd<-unlist(w,use.names=FALSE)
    wid<-rep(seq_along(w),lengths(w))
    once<-!duplicated(data.table::data.table(wid,wd))
    df<-table(wd[once])
    ###the matched name of each pair: the name of the reference entity that gives the pair's key
    pm<-d$nm[match(paste(pid,mk),paste(d$id,d$k))]
    up<-base::unique(pm[!is.na(pm)])
    uw<-strsplit(up," ",fixed=TRUE)
    x<-unique(data.table::data.table(i=rep(seq_along(up),lengths(uw)),w=unlist(uw,use.names=FALSE)))
    x$f<-as.vector(df[match(x$w,names(df))])
    ###N * product of the shares, on the log scale, and the count of the rarest word, per name
    agg<-x[,list(ns=N*exp(sum(log(f/N))),rare=as.integer(min(f))),by=i]
    ns<-rep(NA_real_,length(up))
    rare<-rep(NA_integer_,length(up))
    ns[agg$i]<-agg$ns
    rare[agg$i]<-agg$rare
    list(ns=ns[match(pm,up)],rare=rare[match(pm,up)])
  }
  ds<-if(reference=="b") distinct(pb,kb) else distinct(pa,ka)
  q_namesakes<-ds$ns
  q_rarest_word<-ds$rare
  not_distinct<-(q_namesakes>=namesakes_max)%in%TRUE
  short<-!is.na(mk)&nchar(mk)<short_key

  ###5. entity type: from the type columns where they say person or organisation; otherwise from a first name
  ###in the first_names table (names without a legal form only); otherwise guessed by the rule
  type_of<-function(field,name,lf,cc){
    f<-toupper(field)
    t<-ifelse(!is.na(f)&grepl(org_pattern,f),"organisation",ifelse(!is.na(f)&grepl(person_pattern,f),"person",NA))
    src<-ifelse(is.na(t),"guessed","field")
    if(!is.null(first_names)&&n>0){
      g<-guess_person(name,cc,first_names,min_share=first_name_share,min_count=first_name_count)$type
      use<-is.na(t)&!has(lf)&!is.na(g)
      t[use]<-g[use]
      src[use]<-"first name"
    }
    nm<-stringr::str_squish(toupper(ifelse(is.na(name),"",name)))
    words<-lengths(strsplit(nm," ",fixed=TRUE))
    guess<-ifelse(!has(lf)&words>=2&words<=4&!grepl("[0-9]",nm)&!grepl(company_words,nm),"person","organisation")
    t[is.na(t)]<-guess[is.na(t)]
    list(type=t,src=src)
  }
  ta<-type_of(col(type_a),col(name_a),lfa,col(country_a))
  tb<-type_of(col(type_b),col(name_b),lfb,col(country_b))
  ###the type of the pair: from both sides, or from one side only (for example the IP side against Orbis)
  q_type_pair<-switch(type_side,both=ifelse(ta$type==tb$type,ta$type,"mixed"),a=ta$type,b=tb$type)
  person<-q_type_pair=="person"
  mixed<-q_type_pair=="mixed"
  ###person-like name: a risk signal, not a classification. The side is of type person, or its name contains a
  ###word that is a first name at least person_like_share of the time (also in names with company words)
  person_like_side<-function(t,name,cc){
    fn_hit<-if(!is.null(first_names)&&n>0) !is.na(guess_person(name,cc,first_names,min_share=person_like_share,
                                                                min_count=first_name_count)$first_name) else rep(FALSE,n)
    t$type=="person"|fn_hit
  }
  pl_a<-person_like_side(ta,col(name_a),col(country_a))
  pl_b<-person_like_side(tb,col(name_b),col(country_b))
  q_person_like<-switch(type_side,both=pl_a|pl_b,a=pl_a,b=pl_b)

  ###6. class
  ###a close call: not the best candidate, or the best by less than margin_min, or no score to tell
  close<-ambiguous&(q_best%in%FALSE|is.na(q_margin)|q_margin<margin_min)
  weak<-q_agree==0&(shared|short|q_alias%in%TRUE|mixed|q_names_differ%in%TRUE|q_person_like|not_distinct)
  q_class<-ifelse(conflict,"D",ifelse(close|weak,"C",ifelse(q_agree>=1,"A","B")))

  ###reasons in words: one phrase per finding, without numbers, so that they can be counted
  why<-function(cond,text) ifelse(cond,text,NA)
  src<-switch(type_side,both=ifelse(ta$src=="guessed"|tb$src=="guessed","guessed",
                                     ifelse(ta$src=="first name"|tb$src=="first name","first name","field")),
              a=ta$src,b=tb$src)
  type_label<-if(type_side=="both") ifelse(person,"both persons",ifelse(mixed,"person and organisation","both organisations")) else q_type_pair
  parts<-cbind(
    paste0(type_label,ifelse(src=="guessed"," (type guessed)",ifelse(src=="first name"," (type from first name)",""))),
    why(q_conflict_lf%in%TRUE,"legal forms differ"),
    why(q_conflict_region%in%TRUE,"regions differ"),
    why(q_conflict_city%in%TRUE,"cities differ"),
    why(q_names_differ%in%TRUE,"original names differ"),
    why(q_evidence==0,"name only"),
    why(q_evidence>0&q_agree==0,"no agreeing evidence"),
    why(q_agree>0,paste0("agreeing: ",
        vapply(seq_len(n),function(i) paste(c("legal form","region","city","street")[c(ag_lf[i],ag_reg[i],ag_city[i],ag_street[i])],collapse=", "),""))),
    why(ambiguous&q_best%in%FALSE,"not the best candidate"),
    why(ambiguous&q_best%in%TRUE&q_margin<margin_min,"close call"),
    why(ambiguous&is.na(q_margin),"several candidates, no score"),
    why((q_key_freq_a>1)%in%TRUE,"name shared in A"),
    why((q_key_freq_b>1)%in%TRUE,"name shared in B"),
    why(short,"short name"),
    why(q_alias%in%TRUE,"matched through part of a name"),
    why(q_person_like,"person-like name"),
    why(not_distinct,"name not distinctive")
  )
  q_reasons<-if(n>0) apply(parts,1,function(x) paste(x[!is.na(x)],collapse="; ")) else character(0)

  r$q_class<-as.character(q_class)
  r$q_reasons<-q_reasons
  r$q_evidence<-q_evidence
  r$q_agree<-q_agree
  r$q_conflict_lf<-q_conflict_lf
  r$q_conflict_region<-q_conflict_region
  r$q_conflict_city<-q_conflict_city
  r$q_name_sim<-q_name_sim
  r$q_names_differ<-q_names_differ
  r$q_candidates_a<-q_candidates_a
  r$q_candidates_b<-q_candidates_b
  r$q_best<-q_best
  r$q_margin<-q_margin
  r$q_key_freq_a<-q_key_freq_a
  r$q_key_freq_b<-q_key_freq_b
  r$q_alias<-q_alias
  r$q_type_a<-ta$type
  r$q_type_b<-tb$type
  r$q_type_source_a<-ta$src
  r$q_type_source_b<-tb$src
  r$q_type_pair<-q_type_pair
  r$q_person_like<-q_person_like
  r$q_namesakes<-q_namesakes
  r$q_rarest_word<-q_rarest_word
  r
}

## Words that make a name an organisation when the type is guessed (names in capitals, without punctuation)
company_words<-paste0("\\b(COMPANY|CO|CORP|CORPORATION|INC|GROUP|HOLDINGS?|INTERNATIONAL|TRADING|SERVICES?|SYSTEMS?|",
                      "TECHNOLOG[A-Z]*|INDUSTR[A-Z]*|BANK|UNIVERSIT[A-Z]*|INSTITUT[A-Z]*|FOUNDATION|STIFTUNG|ASSOCIATION|",
                      "VEREIN|SOCIETY|LABORATOR[A-Z]*|PHARMA[A-Z]*|SOLUTIONS?|PRODUCTS?|MEDIA|DESIGN|CONSULTING|",
                      "PARTNERS?|BROTHERS|BROS|FRERES|GEBRUEDER|GEBRUDER|HOTEL|RESTAURANT|BAKKERIJ|BAECKEREI|BACKEREI|",
                      "GMBH|AG|KG|OHG|UG|SE|SA|SAS|SARL|SRL|SPA|SNC|BV|NV|VOF|LTD|LIMITED|LLC|LLP|PLC|OY|OYJ|AB|AS|ASA|APS|",
                      "SPZOO|SP Z O O|OOD|EOOD|AD|EAD|SRO|KFT|ZRT|DOO|DD|OU|UAB|SIA|LDA|SL|SLU|EPE|AE|IKE|",
                      "[A-Z]*GESELLSCHAFT|GENOSSENSCHAFT|SOCIETE|SOCIETA|SOCIEDAD[A-Z]*|SPOLKA|COOPERATIVE?|COOPERATIVA)\\b|&")

## The keys nmatch() matches on, for many names at once: words split, sorted (in the same order as sort(), which
## nmatch() uses), repeated words dropped, pasted together without spaces; NA for missing and empty names
match_keys<-function(x,sort=TRUE,unique=TRUE){
  x<-as.character(x)
  u<-base::unique(x)
  w<-stringr::str_split(u,"\\s")
  d<-data.table::data.table(i=rep(seq_along(w),lengths(w)),w=unlist(w,use.names=FALSE))
  d<-d[!is.na(d$w)&d$w!="",]
  if(sort){
    ###rank the words as sort() orders them (locale collation), then order by name and rank
    lv<-base::sort(base::unique(d$w))
    d$rank<-match(d$w,lv)
    d<-d[order(d$i,d$rank),]
  }
  if(unique) d<-d[!duplicated(data.table::data.table(d$i,d$w)),]
  agg<-d[,list(k=paste(w,collapse="")),by=i]
  k<-rep(NA_character_,length(u))
  k[agg$i]<-agg$k
  k[match(x,u)]
}

## The words of each string sorted as sort() orders them, joined with spaces, for many strings at once
sort_words<-function(x){
  u<-base::unique(x)
  w<-strsplit(u," ",fixed=TRUE)
  d<-data.table::data.table(i=rep(seq_along(w),lengths(w)),w=unlist(w,use.names=FALSE))
  lv<-base::sort(base::unique(d$w))
  d<-d[order(d$i,match(d$w,lv)),]
  agg<-d[,list(k=paste(w,collapse=" ")),by=i]
  k<-rep("",length(u))
  k[agg$i]<-agg$k
  k[match(x,u)]
}
