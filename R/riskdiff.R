#' Cumulative Risk plots
#'
#' Plot comparing two treatments tested in multiple identical locations from long format data
#' @param outcome vector containing numeric outcome values
#' @param plotID ID variable for plot/rep/individual
#' @param trtID  ID variable for treatment
#' @param grpID optional ID variable for grouping factor to set colors by
#' @param trt1 string indicating which treatment to plot on x axis
#' @param trt2 string indicating which treatment to plot on y axis
#' @param approx  Add in approximation line (defaults to FALSE)
#' @param empirical  Add in empirical CDF line (defaults to TRUE)
#' @param same TRUE=overlay mutliple curves FALSE= split into 1 plot per group
#' @param splitlab string for labelling split variable
#' @param linewidth width of lines
#' @param pointsize size of point
#' @keywords cumulative risk
#' @export
#' @examples
#' library(agricolae)
#' date(plrv)
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

riskdiff<-function(outcome,plotID,trtID,grpID=NULL,trt1,trt2,main="",approx=F,empirical=TRUE,linewidth=1.5,pointsize=1){
  #Assess Variability
  require(ggplot2)
  data<-data.frame(outcome=outcome,plotID=plotID,trtID=trtID)
  require(reshape2)
  if(is.null(grpID)){
    
    
    widefmt<-dcast(data,formula(paste("plotID","~","trtID")),value.var="outcome")
    widefmt<-widefmt[,c(trt1,trt2)]
    colnames(widefmt)<-c("trt1","trt2")
    widefmt<-na.omit(widefmt)

    riskplot(widefmt$trt1-widefmt$trt2,xlab=paste(trt1,"-",trt2),main=main,approx=approx,
             empirical=empirical,linewidth=linewidth,pointsize=pointsize)+
      annotate(geom="text",x=0,y=105,label=paste("Higher for\n",trt2,sep=""),hjust=1.1,size=3)+
      annotate(geom="text",x=0,y=105,label=paste("Higher for\n",trt1,sep=""),hjust=-0.1,size=3)
  }
  else{
    data$grpID<-grpID
    widefmt<-dcast(data,formula(paste("plotID","+","grpID","~","trtID")),value.var="outcome")
    widefmt<-widefmt[,c("grpID",trt1,trt2)]
    colnames(widefmt)<-c("group","trt1","trt2")
    widefmt<-na.omit(widefmt)
    
    riskplot(widefmt$trt1-widefmt$trt2,xlab=paste(trt1,"-",trt2),empirical=empirical,
             splitvar=widefmt$group,splitlab=deparse(substitute(grpID)),main=main,approx=approx,linewidth=linewidth,pointsize=pointsize)+
      annotate(geom="text",x=0,y=105,label=paste("Higher for\n",trt2,sep=""),hjust=1.1,size=3)+
      annotate(geom="text",x=0,y=105,label=paste("Higher for\n",trt1,sep=""),hjust=-0.1,size=3)
  } 
  
}
