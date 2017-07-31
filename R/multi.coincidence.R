#' Produces a coincidence matrix plot from a multiresponse variable
#' 
#' Percentages will rarely add up to 100, unless exactly two variables are selected in the multi-response 
#' @param df data frame with the binary variables
#' @param scaleby "none" to show counts, "total" to show overall percentages, "row" to show within row percentages, "col" to show within column percentages
#' @param diagonal values to place on diagonals. "none" for no values, "sole" for the responses to the category where it was the only response, "all" for all responses to the category
#' @param order order of responses. "frequency" for frequency order; "alpha" for
#'   alphabetical; "original" for original column order
#' @param show "all" to show entire matrix, "upper" to show upper half only, "lower" to show lower half only 
#' @param tidynames tidy the response names to remove . or _ added to variable
#'   names brought in through import or use of split.multi()
#' @param text include text labels within cells (TRUE/FALSE)
#' @param textsize size for text labels (defaults to 3)
#' @param low string indicating of colour to use for lowest value (defaults to "white")
#' @param low string indicating of colour to use for highest value (defaults to "red")
#' @keywords multiresponse graph
#' @export
#' @examples
#' sampledata<-data.frame(Response1=sample(c(0,1),100,replace=TRUE),
#' Response2=sample(c(0,1),100,replace=TRUE),
#' Response3=sample(c(0,1),100,replace=TRUE),Category=sample(c("A","B","C"),100,replace=TRUE))
#' multi.coincidence(sampledata[,c("Response1","Response2","Response3")])
#' multi.coincidence(sampledata[,c("Response1","Response2","Response3")],scaleby="total",diagonal="sole",show="lower")

multi.coincidence<-function(df,scaleby="none",diagonal="sole",order="frequency",text=TRUE,textsize=3,
                            low="white",high="red",show="all",tidynames=TRUE){
  require(questionr)  
  require(ggplot2)  
  require(reshape2)
  
  coincidence1<-matrix(nrow=ncol(df),ncol=ncol(df),data = 0)
  for(i in 1:nrow(coincidence1)){
    for(j in 1:nrow(coincidence1)){
      
      if(i!=j|(diagonal=="all"&i==j)){
        coincidence1[i,j]<-sum(df[,i]==1&df[,j]==1)
      }
      if(i==j&diagonal=="sole"){
        coincidence1[i,j]<-sum(df[,i]==1&rowSums(df)==1)
      }  
      if(i==j&diagonal=="none"){
        coincidence1[i,j]<-0
      } 
      
    }
  }
  
  
  colnames(coincidence1)<-rownames(coincidence1)<-colnames(df)  
  if(scaleby=="row"){
    coincidence1<-100*coincidence1/rep(colSums(df),each=ncol(df)) 
  }   
  if(scaleby=="col"|scaleby=="column"){
    coincidence1<-100*coincidence1/rep(colSums(df),times=ncol(df)) 
  } 
  if(scaleby=="total"){
    coincidence1<-100*coincidence1/nrow(df)
  }
  
  coincidence2<-melt(coincidence1)
  
  freqs<-data.frame(multi.table(df=df))
  freqs$response<-rownames(freqs)
  
  if(order=="frequency"){
    if(tfreq=="row"){levs<-rownames(freqs)[order(freqs[,1])]}
    if(tfreq=="col"){levs<-rownames(freqs)[order(freqs[,1],decreasing = TRUE)]}
    
    coincidence2$Var1<-factor(coincidence2$Var1,levels=levs)
    coincidence2$Var2<-factor(coincidence2$Var2,levels=levs)
  }
  if(order=="original"){
    levs<-rownames(freqs) 
    coincidence2$Var1<-factor(coincidence2$Var1,levels=levs)
    coincidence2$Var2<-factor(coincidence2$Var2,levels=levs)
  }
  #All other inputs to order result in alphabetical ordering of levels
  if(order!="original"&order!="frequency"){
    coincidence2$Var1<-factor(coincidence2$Var1)
    coincidence2$Var2<-factor(coincidence2$Var2)
    
  }
  
  
  if(show=="upper"){
    coincidence2<-subset(coincidence2,as.numeric(Var1)<=as.numeric(Var2))
  }
  if(show=="lower"){
    coincidence2<-subset(coincidence2,as.numeric(Var1)>=as.numeric(Var2))
  }
  
  if(tidynames==TRUE){
    levels(coincidence2$Var1)<-gsub("\\.|_"," ",levels(coincidence2$Var1))
    levels(coincidence2$Var2)<-gsub("\\.|_"," ",levels(coincidence2$Var2))
  }
  
  
  
  p1<-ggplot(data=coincidence2,aes(x=Var1,y=Var2,fill=value))+geom_tile(col="black")+theme(axis.text.x = element_text(angle=90))+ylab("")+xlab("")+
    scale_fill_continuous(name=ifelse(scaleby=="none","Count",ifelse(scaleby=="row","% of Response (By Row)",
                                                                     ifelse(scaleby=="col"|scaleby=="column","% of Response (By Column)",
                                                                            "% of All Responses"))),
                          low=low,high = high)
  if(text==TRUE&scaleby=="none"){
    p1<-p1+geom_text(aes(label=ifelse(value==0,"",paste("n=",value,sep=""))),size=textsize)
  }
  if(text==TRUE&scaleby!="none"){
    p1<-p1+geom_text(aes(label=ifelse(value==0,"",paste(round(value,0),"%",sep=""))),size=textsize)
  }
  return(p1)
}