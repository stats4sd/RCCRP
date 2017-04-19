#' Compare Treatments
#'
#' Plot comparing two treatments tested in multiple identical locations from long format data
#' @param outcome vector containing numeric outcome values
#' @param plotID ID variable for plot/rep/individual
#' @param trtID  ID variable for treatment
#' @param grpID optional ID variable for grouping factor to set colors by
#' @param trt1 string indicating which treatment to plot on x axis
#' @param trt2 string indicating which treatment to plot on y axis
#' @param smoother add geom with loess smoother
#' @param meanline add geom with overall mean difference parallel to y=x
#' @keywords Compare Plot
#' @export
#' @examples
#' library(agricolae)
#' data(plrv)
#' 
#' #example without groups
#' compare_trt(outcome=plrv$Yield,
#' plotID=paste(plrv$Locality,plrv$Rep),
#' trtID=plrv$Genotype,
#' trt1="Canchan",
#' trt2="Desiree")
#' 
#' #example with groups
#' compare_trt(outcome=plrv$Yield,
#' plotID=paste(plrv$Locality,plrv$Rep),
#' trtID=plrv$Genotype,
#' grpID=plrv$Locality,
#' trt1="Canchan",
#' trt2="Desiree")

compare_trt<-function(outcome,plotID,trtID,grpID=NULL,trt1,trt2,smoother=T,meanline=T){
  #Assess Variability
  
  data<-data.frame(outcome=outcome,plotID=plotID,trtID=trtID)

  require(ggplot2)
  require(reshape2)
  if(is.null(grpID)){
    
    
    widefmt<-dcast(data,formula(paste("plotID","~","trtID")),value.var="outcome",fun.aggregate = mean,na.rm=T)
    widefmt<-widefmt[,c(trt1,trt2)]
    colnames(widefmt)<-c("trt1","trt2")
    widefmt<-na.omit(widefmt)
    
    p1<-ggplot(aes(y=trt2,x=trt1),data=widefmt)+geom_point(size=2)+
           geom_abline()+xlim(c(min(widefmt,na.rm=T),max(widefmt,na.rm=T)))+
           ylim(c(min(widefmt,na.rm=T),max(widefmt,na.rm=T)))+ylab(trt2)+xlab(trt1)+
           ggtitle(paste("Comparison Plot of",trt2,"against",trt1))
    
  }
  else{
    data$grpID<-grpID
    widefmt<-dcast(data,formula(paste("plotID","+","grpID","~","trtID")),value.var="outcome",fun.aggregate = mean,na.rm=T)
    widefmt<-widefmt[,c("grpID",trt1,trt2)]
    colnames(widefmt)<-c("group","trt1","trt2")
    widefmt<-na.omit(widefmt)
    
    p1<-ggplot(aes(y=trt2,x=trt1),data=widefmt)+geom_point(size=2,aes(colour=group))+
           geom_abline()+xlim(c(min(widefmt[,-1],na.rm=T),max(widefmt[,-1],na.rm=T)))+
           ylim(c(min(widefmt[,-1],na.rm=T),max(widefmt[,-1],na.rm=T)))+ylab(trt2)+xlab(trt1)+
           ggtitle(paste("Comparison Plot of",trt2,"against",trt1,"by",deparse(substitute(grpID))))+
                     scale_color_discrete(name=deparse(substitute(grpID)))

      } 
  
  
  if(smoother==T){p1<-p1+geom_smooth(se=F,method="loess",alpha=0.5)}
  if(meanline==T){p1<-p1+geom_abline(slope = 1,intercept=mean(widefmt$trt2-widefmt$trt1,na.rm=T),col="red")}
  p1
}

