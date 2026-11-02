#' @title Summarise the quality flags of matched pairs
#'
#' @description Counts the pairs per quality class and entity type, and how often each reason occurs, from the
#' output of \code{\link{match_quality}}. Useful for the method documentation and to compare deliveries of data.
#' @param q the output of \code{\link{match_quality}}
#' @returns a list of two data frames: \code{classes}, the pairs per class (A to D) for organisations, persons and
#' mixed pairs, with the total and its share in percent; and \code{reasons}, for each reason the pairs per class,
#' the total and its share of all pairs in percent, most frequent first. In \code{reasons}, the entity type is left
#' out (it is in \code{classes}) and the agreeing signals are counted together as "agreeing evidence".
#' @examples
#' pairs <- data.frame(id_a = c(1, 2, 3, 3), id_b = c(11, 12, 13, 14), norm_name = c("ALPHA", "DEJANVRIES", "BETA", "BETA"),
#'                     name_a = c("Alpha GmbH", "Jan de Vries", "Beta AG", "Beta AG"),
#'                     name_b = c("ALPHA GMBH", "JAN DE VRIES", "BETA AG", "BETA AKTIENGESELLSCHAFT"),
#'                     one_to_one = TRUE, name_sim = c(1, 1, 1, 0.77), city_sim = 0, street_sim = 0,
#'                     reg_a = c("DE212", NA, "DE300", "DE300"), reg_b = c("DE212", NA, "DE300", "DEA23"),
#'                     reg_sim = c(TRUE, FALSE, TRUE, FALSE), final_score = c(0.8, 0.4, 0.8, 0.49))
#' quality_summary(match_quality(pairs, "id_a", "id_b", "name_a", "name_b"))
#' @export

quality_summary<-function(q){
  q<-as.data.frame(q)
  miss<-setdiff(c("q_class","q_reasons","q_type_pair"),names(q))
  if(length(miss)>0) stop("quality_summary: q lacks the columns ",paste(miss,collapse=", ")," (use the output of match_quality())")
  lv<-c("A","B","C","D")
  cls<-factor(q$q_class,levels=lv)
  tp<-factor(q$q_type_pair,levels=c("organisation","person","mixed"))
  t<-table(cls,tp)
  classes<-data.frame(class=lv,organisation=as.vector(t[,"organisation"]),person=as.vector(t[,"person"]),
                      mixed=as.vector(t[,"mixed"]))
  classes$total<-classes$organisation+classes$person+classes$mixed
  classes$share<-round(100*classes$total/max(1,sum(classes$total)),1)

  ###each reason once per pair; the entity type is in classes, the agreeing signals are counted together
  rs<-strsplit(as.character(ifelse(is.na(q$q_reasons),"",q$q_reasons)),"; ",fixed=TRUE)
  rs<-lapply(rs,function(x){
    x<-x[!grepl("^both |^person and organisation",x)&x!=""]
    base::unique(sub("^agreeing: .*","agreeing evidence",x))
  })
  d<-data.frame(class=factor(rep(as.character(cls),lengths(rs)),levels=lv),reason=as.character(unlist(rs,use.names=FALSE)))
  rt<-table(d$reason,d$class)
  reasons<-data.frame(reason=as.character(rownames(rt)),stringsAsFactors=FALSE)
  for(k in lv) reasons[[k]]<-as.vector(rt[,k])
  reasons$total<-rowSums(reasons[,lv,drop=FALSE])
  reasons$share<-round(100*reasons$total/max(1,nrow(q)),1)
  reasons<-reasons[order(-reasons$total,reasons$reason),]
  rownames(reasons)<-NULL
  list(classes=classes,reasons=reasons)
}
