#' Produces a barchart from a multiple response variable with a cross tabulation variable. 
#' 
#' For column percentages or frequencies the multiresponses are facetted; for row percentages the multiresponses are stacked
#' Operates in similar way to functions included in questionr, and requires same inputs 
#' @param df data frame with the binary variables
#' @param crossvar factor to cross the multiple choices question with
#' @param freq use percentages
#' @param tfreq type of percentages to compute ("row" or "col")
#' @param color Use color scale for cross factor or rely on position only
#' @param crossname label for cross factor
#' @param order order of responses. "frequency" for frequency order; "alpha" for
#'   alphabetical; "original" for original column order
#' @param tidynames tidy the response names to remove . or _ added to variable
#'   names brought in through import or use of split.multi()
#' @param multiname label for multiple selection variable
#' @param total include total row for row-wise plot
#' @keywords multiresponse graph
#' @export
#' @examples
#' sampledata<-data.frame(Response1=sample(c(0,1),100,replace=TRUE),
#' Response2=sample(c(0,1),100,replace=TRUE),
#' Response3=sample(c(0,1),100,replace=TRUE),Category=sample(c("A","B","C"),100,replace=TRUE))
#' cross.multi.barplot(sampledata[,c("Response1","Response2","Response3")],sampledata$Category,order="original")
#' cross.multi.barplot(sampledata[,c("Response1","Response2","Response3")],sampledata$Category,order="original",tfreq="row")
#' cross.multi.barplot(sampledata[,c("Response1","Response2","Response3")],sampledata$Category,order="original",freq=FALSE)

cross.multi.barplot<-function(df,crossvar,freq=TRUE,tfreq="col",color=TRUE,crossname=NULL,order="frequency",tidynames=TRUE,multiname=NULL,total=TRUE,...){
require(questionr)  
require(ggplot2)  
require(reshape2)
  
  x<-data.frame(cross.multi.table(df=df,crossvar =crossvar,freq=freq,tfreq = tfreq,...))
  x$response<-rownames(x)
  
  y<-melt(x,id.vars="response")
  freqs<-data.frame(multi.table(df=df))
  freqs$response<-rownames(freqs)
  
  if(order=="frequency"){
    if(tfreq=="row"){levs<-rownames(freqs)[order(freqs[,1])]}
    if(tfreq=="col"){levs<-rownames(freqs)[order(freqs[,1],decreasing = TRUE)]}
    
    y$response<-factor(y$response,levels=levs)
  }
  if(order=="original"){
    levs<-rownames(freqs) 
    y$response<-factor(y$response,levels=levs)
  }
  #All other inputs to order result in alphabetical ordering of levels
  if(order!="original"&order!="frequency"){
    y$response<-factor(y$response)
    
  }
  
  #make sure variable inherits original ordering of levels if it was a factor
  
  if(is.factor(crossvar)==TRUE){
    y$variable<-factor(y$variable,levels=levels(crossvar))
  }
  
  
  if(tidynames==TRUE){
    levels(y$response)<-gsub("\\.|_"," ",levels(y$response))
  }
  if(freq==FALSE){
    if(color==TRUE){p1<-ggplot(data=y,aes(y=value,x=variable,fill=variable))+geom_bar(stat="identity",col="black")+facet_wrap(~response)+ylab("Count")
    if(is.null(crossname)==FALSE){p1<-p1+scale_fill_discrete(name=crossname)+xlab(crossname)}
    }
    if(color==FALSE){p1<-ggplot(data=y,aes(y=value,x=variable))+geom_bar(stat="identity",col="black")+facet_wrap(~response)+ylab("Count")
    if(is.null(crossname)==FALSE){p1<-p1+xlab(crossname)}}
  }
    
  if(freq==TRUE){
    if(tfreq=="col"){
      if(color==TRUE){p1<-ggplot(data=y,aes(y=value,x=variable,fill=variable))+geom_bar(stat="identity",col="black")+facet_wrap(~response)+ylab("Percentage")
      if(is.null(crossname)==FALSE){p1<-p1+scale_fill_discrete(name=crossname)+xlab(crossname)}
      }
      if(color==FALSE){p1<-ggplot(data=y,aes(y=value,x=variable))+geom_bar(stat="identity",col="black")+facet_wrap(~response)+ylab("Percentage")
      if(is.null(crossname)==FALSE){p1<-p1+xlab(crossname)}}
      
    }
    
    if(tfreq=="row"){
      width=1
      if(total==TRUE){
        
        ytot<-data.frame(response="Total Population",100*prop.table(table(crossvar)))
        colnames(ytot)[2:3]<-c("variable","value")
        
        
        y1<-merge(y,ytot,all=TRUE)
        
        y1
        
        
        y1$response<-factor(y1$response,levels=c(levels(y$response),"Total Population"))
        
        width<-c(rep(1,nrow(x)*(ncol(x)-1)),rep(1.75,ncol(x)-1))
        
        pos<-rep(c(1:nrow(x),nrow(x)+1.75),each=ncol(x)-1)
        
      }
      
      
      if(color==TRUE){p1<-ggplot(data=y1,aes(y=value,x=pos,fill=variable))+
        geom_bar(stat="identity",position="stack",col="black",width=width)+ylab("Percentage")+coord_flip()+
        scale_x_continuous(breaks=c(1:nrow(x),nrow(x)+1.75),labels=levels(y1$response))
      p1<-p1+scale_fill_discrete(name=ifelse(is.null(crossname),"",crossname))
      p1<-p1+xlab(ifelse(is.null(multiname),"",multiname))
      }
      if(color==FALSE){p1<-ggplot(data=y1,aes(y=value,x=pos,fill=variable))+geom_bar(stat="identity",position="stack",col="black",width=width)+
        ylab("Percentage")+coord_flip()+scale_x_continuous(breaks=c(1:nrow(x),nrow(x)+1.75),labels=levels(y1$response))
      p1<-p1+scale_fill_grey(start = 0.4, end = 0.8,name=ifelse(is.null(crossname),"",crossname))
      p1<-p1+xlab(ifelse(is.null(multiname),"",multiname))
      }
      
      
    }
    
  }
  
  return(p1)
  
}
