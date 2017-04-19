#' Cumulative Risk plots
#'
#' Plot comparing two treatments tested in multiple identical locations from long format data
#' @param outcome vector containing numeric outcome values
#' @param plotID ID variable for plot/rep/individual
#' @param trtID  ID variable for treatment
#' @param grpID optional ID variable for grouping factor to set colors by
#' @param trt1 string indicating which treatment to plot on x axis
#' @param trt2 string indicating which treatment to plot on y axis
#' @param approx  Add in approximation line (defaults to TRUE)
#' @param same TRUE=overlay mutliple curves FALSE= split into 1 plot per group
#' @param splitlab string for labelling split variable
#' @keywords cumulative risk
#' @export
#' @examples
#' riskdiff(outcome=plrv$Yield,
#' plotID=paste(plrv$Locality,plrv$Rep,sep=""),
#' trtID=plrv$Genotype,
#' trt1="Canchan",trt2="Unica",
#' main="Comparison of Yield: Canchan vs Unica")
#' 
#' riskdiff(outcome=plrv$Yield,
#' plotID=paste(plrv$Locality,plrv$Rep,sep=""),
#' trtID=plrv$Genotype,
#' grpID=plrv$Locality,
#' trt1="Canchan",trt2="Unica",
#' main="Comparison of Yield: Canchan vs Unica")

riskdiff<-function(outcome,plotID,trtID,grpID=NULL,trt1,trt2,main="",approx=F){
  #Assess Variability
  require(ggplot2)
  data<-data.frame(outcome=outcome,plotID=plotID,trtID=trtID)
  require(reshape2)
  if(is.null(grpID)){
    
    
    widefmt<-dcast(data,formula(paste("plotID","~","trtID")),value.var="outcome")
    widefmt<-widefmt[,c(trt1,trt2)]
    colnames(widefmt)<-c("trt1","trt2")
    widefmt<-na.omit(widefmt)

    riskplot(widefmt$trt1-widefmt$trt2,xlab=paste(trt1,"-",trt2),main=main,approx=approx)+
      annotate(geom="text",x=0,y=105,label=paste(trt1," >"),hjust=1.1)+annotate(geom="text",x=0,y=105,label=paste(trt2," >"),hjust=-0.1)
  }
  else{
    data$grpID<-grpID
    widefmt<-dcast(data,formula(paste("plotID","+","grpID","~","trtID")),value.var="outcome")
    widefmt<-widefmt[,c("grpID",trt1,trt2)]
    colnames(widefmt)<-c("group","trt1","trt2")
    widefmt<-na.omit(widefmt)
    
    riskplot(widefmt$trt1-widefmt$trt2,xlab=paste(trt1,"-",trt2),splitvar=widefmt$group,splitlab=deparse(substitute(grpID)),main=main,approx=approx)+
      annotate(geom="text",x=0,y=105,label=paste(trt1," >"),hjust=1.1)+annotate(geom="text",x=0,y=105,label=paste(trt2," >"),hjust=-0.1)
  } 
  
}
