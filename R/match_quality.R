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
#'   person against organisation, original names that differ, or a person whose name frequency is unknown}
#'   \item{D, conflicting}{at least one conflict: legal forms with nothing in common, different regions, or different
#'   cities}
#' }
#' The entity type is taken from \code{type_a} and \code{type_b} where they say person or organisation; otherwise
#' it is guessed: a person has no legal form and a name of two to four words without digits, company words or
#' legal-form words. The original names are compared with their words sorted (\code{q_name_sim}), so that a surname
#' first equals a surname last; differing names are only a weak signal, because normalisation removes words on
#' purpose. Without \code{norm_a} and \code{norm_b}, matches through part of a name cannot be recognised and their
#' names show as differing.
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
#' \code{q_type_b}, \code{q_type_source_a}, \code{q_type_source_b} (field or guessed) and \code{q_type_pair}
#' (organisation, person or mixed)
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
                        type_a=NULL,type_b=NULL,name_sim_min=0.8,city_sim_min=0.8,street_sim_min=0.8,
                        margin_min=0.05,short_key=5,
                        person_pattern="INDIVIDUAL|NATURAL|PHYSICAL|PERSON",
                        org_pattern="COMPANY|LEGAL|ENTITY|GOV|UNIVERSIT|HOSPITAL|ORGANI|NON-PROFIT"){
  r<-as.data.frame(pairs)
  need<-c(key_a,key_b,name_a,name_b,match_key,"one_to_one","name_sim","city_sim","street_sim","reg_a","reg_b","reg_sim")
  miss<-setdiff(need,names(r))
  if(length(miss)>0) stop("match_quality: pairs lacks the columns ",paste(miss,collapse=", "),
                          " (use the output of clean() or rankmatch())")
  n<-nrow(r)
  ###a column as text, or NA when the column is not given
  col<-function(x) if(is.null(x)) rep(NA_character_,n) else as.character(r[[x]])
  has<-function(x) !is.na(x)&stringr::str_trim(x)!=""
  ###values separated by sep, as a list of sets ("GMBH;KG", "DE212, DE300")
  sets<-function(x,sep) lapply(strsplit(ifelse(is.na(x),"",x),sep,fixed=TRUE),function(s){s<-stringr::str_trim(s); unique(s[s!=""])})
  overlap<-function(x,y) vapply(seq_len(n),function(i) length(intersect(x[[i]],y[[i]]))>0,TRUE)

  ###1. evidence: the signals present on both sides, and those that agree
  lfa<-col(lf_a)
  lfb<-col(lf_b)
  ev_lf<-has(lfa)&has(lfb)
  ev_reg<-(has(r$reg_a)&has(r$reg_b))|(has(col(zip_a))&has(col(zip_b)))
  ev_city<-has(col(city_a))&has(col(city_b))
  ev_street<-has(col(street_a))&has(col(street_b))
  lf_common<-overlap(sets(lfa,";"),sets(lfb,";"))
  reg_common<-r$reg_sim%in%TRUE|overlap(sets(r$reg_a,","),sets(r$reg_b,","))
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
    cand<-tapply(o,g,function(v) length(unique(v)))
    cand<-as.integer(cand[g])
    best<-rep(NA,n)
    margin<-rep(NA_real_,n)
    multi<-which(cand>1&!is.na(score))
    for(idx in split(multi,g[multi])){
      ###best score of each candidate (a candidate can appear in several rows)
      s<-tapply(score[idx],o[idx],max)
      s<-sort(s,decreasing=TRUE)
      best[idx]<-score[idx]>=s[1]&o[idx]==names(s)[1]
      margin[idx]<-s[1]-s[2]
    }
    best[!is.na(cand)&cand==1]<-TRUE
    list(cand=cand,best=best,margin=margin)
  }
  sa<-side(ka,kb)
  sb<-side(kb,ka)
  q_candidates_a<-sa$cand
  q_candidates_b<-sb$cand
  q_best<-sa$best&sb$best
  q_margin<-pmin(sa$margin,sb$margin,na.rm=TRUE)
  ambiguous<-(q_candidates_a>1)%in%TRUE|(q_candidates_b>1)%in%TRUE|!(r$one_to_one%in%TRUE)

  ###4. names: how many entities share the name, short names, matches through part of a name
  key_of<-function(x){
    x<-as.character(x)
    u<-base::unique(x)
    k<-vapply(stringr::str_split(u,"\\s"),function(w){
      if(sort==T) w<-base::sort(w)
      if(unique==T) w<-stringi::stri_unique(w)
      paste(w,collapse="")
    },"")
    k[is.na(u)|k==""]<-NA
    k[match(x,u)]
  }
  mk<-as.character(r[[match_key]])
  freq<-function(nt,idcol,namecol){
    if(is.null(nt)) return(rep(NA_integer_,n))
    nt<-as.data.frame(nt)
    d<-unique(data.frame(id=as.character(nt[[idcol]]),k=key_of(nt[[namecol]])))
    d<-d[!is.na(d$k),]
    cnt<-table(d$k)
    as.integer(cnt[match(mk,names(cnt))])
  }
  q_key_freq_a<-freq(norm_a,norm_key_a,norm_name_a)
  q_key_freq_b<-freq(norm_b,norm_key_b,norm_name_b)
  alias<-function(nt,idcol,namecol,pid){
    if(is.null(nt)) return(rep(NA,n))
    nt<-as.data.frame(nt)
    d<-unique(data.frame(id=as.character(nt[[idcol]]),nm=as.character(nt[[namecol]])))
    d<-d[!is.na(d$nm)&d$nm!="",]
    d$k<-key_of(d$nm)
    ###a name is a part when another name of the same entity contains it as whole words
    multi<-d[d$id%in%d$id[duplicated(d$id)],]
    j<-merge(multi,multi,by="id",suffixes=c("","_long"))
    j<-j[j$nm!=j$nm_long&stringi::stri_detect_fixed(paste0(" ",j$nm_long," "),paste0(" ",j$nm," ")),]
    out<-paste(pid,mk)%in%paste(j$id,j$k)
    out[!pid%in%d$id]<-NA
    out
  }
  al_a<-alias(norm_a,norm_key_a,norm_name_a,ka)
  al_b<-alias(norm_b,norm_key_b,norm_name_b,kb)
  q_alias<-ifelse(al_a%in%TRUE|al_b%in%TRUE,TRUE,ifelse(is.na(al_a)&is.na(al_b),NA,FALSE))
  shared<-(q_key_freq_a>1)%in%TRUE|(q_key_freq_b>1)%in%TRUE
  ###similarity of the original names with their words sorted, so that DE VRIES JAN equals JAN DE VRIES;
  ###not checked for matches through part of a name, where the full name differs by nature
  words_sorted<-function(x){
    x<-stringi::stri_trans_general(stringr::str_replace_all(ifelse(is.na(x),"",x),"[^\\p{Latin}\\p{Greek}\\p{Cyrillic}0-9]"," "),"latin-ascii; upper")
    vapply(strsplit(stringr::str_squish(x)," ",fixed=TRUE),function(w) paste(base::sort(w),collapse=" "),"")
  }
  q_name_sim<-stringdist::stringsim(words_sorted(col(name_a)),words_sorted(col(name_b)),method="jw")
  q_names_differ<-ifelse(q_alias%in%TRUE,NA,q_name_sim<name_sim_min)
  short<-!is.na(mk)&nchar(mk)<short_key

  ###5. entity type: from the type columns where they say person or organisation, otherwise guessed
  type_of<-function(field,name,lf){
    f<-toupper(field)
    t<-ifelse(!is.na(f)&grepl(org_pattern,f),"organisation",ifelse(!is.na(f)&grepl(person_pattern,f),"person",NA))
    src<-ifelse(is.na(t),"guessed","field")
    nm<-stringr::str_squish(toupper(ifelse(is.na(name),"",name)))
    words<-lengths(strsplit(nm," ",fixed=TRUE))
    guess<-ifelse(!has(lf)&words>=2&words<=4&!grepl("[0-9]",nm)&!grepl(company_words,nm),"person","organisation")
    t[is.na(t)]<-guess[is.na(t)]
    list(type=t,src=src)
  }
  ta<-type_of(col(type_a),col(name_a),lfa)
  tb<-type_of(col(type_b),col(name_b),lfb)
  q_type_pair<-ifelse(ta$type==tb$type,ta$type,"mixed")
  person<-q_type_pair=="person"
  mixed<-q_type_pair=="mixed"
  freq_unknown<-is.na(q_key_freq_a)|is.na(q_key_freq_b)

  ###6. class
  ###a close call: not the best candidate, or the best by less than margin_min, or no score to tell
  close<-ambiguous&(q_best%in%FALSE|is.na(q_margin)|q_margin<margin_min)
  weak<-q_agree==0&(shared|short|q_alias%in%TRUE|mixed|q_names_differ%in%TRUE|(person&freq_unknown))
  q_class<-ifelse(conflict,"D",ifelse(close|weak,"C",ifelse(q_agree>=1,"A","B")))

  ###reasons in words: one phrase per finding, without numbers, so that they can be counted
  why<-function(cond,text) ifelse(cond,text,NA)
  guessed<-ta$src=="guessed"|tb$src=="guessed"
  parts<-cbind(
    paste0(ifelse(person,"both persons",ifelse(mixed,"person and organisation","both organisations")),
           ifelse(guessed," (type guessed)","")),
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
    why(person&freq_unknown,"name frequency not available")
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
