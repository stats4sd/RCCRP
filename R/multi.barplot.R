#' Produces a barchart from a multiple response variable. 
#' @param df data frame with the binary variables
#' @param type "percent" or "count"
#' @param fill Color(s) to use for bars. Must be single color of vector of length of responses
#' @param order order of responses. "frequency" for frequency order; "alpha" for
#'   alphabetical; "original" for original column order
#' @param tidynames tidy the response names to remove . or _ added to variable
#'   names brought in through import or use of split.multi()
#' @param multiname label for multiple selection variable
#' @keywords multiresponse graph
#' @export
#' @examples
#' sampledata<-data.frame(Response1=sample(c(0,1),100,replace=TRUE),
#' Response2=sample(c(0,1),100,replace=TRUE),
#' Response3=sample(c(0,1),100,replace=TRUE),Category=sample(c("A","B","C"),100,replace=TRUE))
#' multi.barplot(sampledata[,c("Response1","Response2","Response3")],order="original")

multi.barplot<-function(df,order="frequency",type="percent",tidynames=TRUE,multiname=NULL,fill="grey",...){
  require(questionr)  
  require(ggplot2)  
  require(reshape2)
  
  
  freqs<-data.frame(multi.table(df=df,...))
  freqs$response<-rownames(freqs)
  
  if(order=="frequency"){
    if(tfreq=="row"){levs<-rownames(freqs)[order(freqs[,1])]}
    if(tfreq=="col"){levs<-rownames(freqs)[order(freqs[,1],decreasing = TRUE)]}
    
    freqs$response<-factor(freqs$response,levels=levs)
  }
  if(order=="original"){
    levs<-rownames(freqs) 
    freqs$response<-factor(freqs$response,levels=levs)
  }
  #All other inputs to order result in alphabetical ordering of levels
  if(order!="original"&order!="frequency"){
    freqs$response<-factor(freqs$response)
    
  }
  
  #make sure variable inherits original ordering of levels if it was a factor
  
  if(type=="count"){freqs$value<-freqs$n}
  if(type=="percent"){freqs$value<-freqs$X.multi}
  
  if(tidynames==TRUE){
    levels(freqs$response)<-gsub("\\.|_"," ",levels(freqs$response))
  }
  
  p1<-ggplot(data=freqs,aes(y=value,x=response))+
    geom_bar(stat="identity",col="black",fill=fill)+ylab(type)+coord_flip()+xlab(ifelse(is.null(multiname),"",multiname))
  
  
  return(p1)
}

