#' Cumulative Risk plots
#'
#' @param y vector containing numeric outcome values
#' @param splitvar ID variable for group to split by
#' @param approx  Add in approximation line (defaults to TRUE)
#' @param same TRUE=overlay mutliple curves FALSE= split into 1 plot per group
#' @param splitlab string for labelling split variable
#' @keywords cumulative risk
#' @export
#' @examples
#' See riskdiff()

riskplot<-function(y,splitvar=NULL,approx=T,v=0,same=T,splitlab="",main="",xlab="y"){

  require(ggplot2)
  if(class(splitvar)=="NULL"){
    sort1<-order(y)
    y<-na.omit(y[sort1])
    
    y1<-data.frame(y=y)
    
    y1$prop<-100*(1:nrow(y1))/nrow(y1)
    if(approx==T){
      y1$approx<-100*pnorm(y1$y,mean=mean(y1$y,na.rm=T),sd=sd(y1$y,na.rm=T))
    }
    
    p1<-ggplot(data=y1,aes(y=prop,x=y))+geom_step()+geom_point()+ylab("Cumulative Percentage")+geom_vline(xintercept=v)+
      xlab(xlab)+scale_y_continuous(minor_breaks = seq(0 , 100, 5), breaks = seq(0, 100, 10),limits=c(0,105))
    
  
    if(approx==T){ 
      
       p1<-p1+geom_line(aes(y=approx),col=2,data=y1) 
    }
  }
  if(class(splitvar)!="NULL"){
    
    sort1<-order(y)
    y1<-data.frame(y=y,splitvar=splitvar)
    y1<-na.omit(y1[sort1,])
    
    levs<-levels(as.factor(as.character(splitvar)))
    n<-length(levs)
    y1$prop<-NA
    y1$approx<-NA
    for(i in 1:n){
      y1$prop[y1$splitvar==levels(y1$splitvar)[i]]<-100*(1:nrow(y1[y1$splitvar==levels(y1$splitvar)[i],]))/nrow(y1[y1$splitvar==levels(y1$splitvar)[i],])
      
      if(approx==T){
        y1$approx[y1$splitvar==levels(y1$splitvar)[i]]<-100*pnorm(y1$y[y1$splitvar==levels(y1$splitvar)[i]],
                                                                  mean=mean(y1$y[y1$splitvar==levels(y1$splitvar)[i]],na.rm=T),
                                                                  sd=sd(y1$y[y1$splitvar==levels(y1$splitvar)[i]],na.rm=T))
      }
      
    }
    
    p1<-ggplot(data=y1,aes(y=prop,x=y,group=splitvar))+geom_step(aes(col=splitvar))+geom_point(aes(col=splitvar))+
      ylab("Cumulative Percentage")+geom_vline(xintercept=v)+xlab(xlab)+
      scale_y_continuous(minor_breaks = seq(0 , 100, 5), breaks = seq(0, 100, 10),limits=c(0,105))+
      scale_color_discrete(name=splitlab)
    
    
      if(approx==T){
        p1<-p1+geom_line(aes(y=approx,col=splitvar),data=y1) 
      }
    if(same==F){
      p1<-p1+facet_wrap(~splitvar)
    }
    }
   p1+ggtitle(main)
  }
 

