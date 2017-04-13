#' Plot means with confidence intervals from raw data
#'
#' Wrapper so that means with error bars can be plotted in a one-liner with same formula input style as boxplot(), lm(), plot() etc.
#' @param formula a formula of the outcome variable and the explanatory variable(s)
#' @param data data frame containing variables
#' @param conf.level defaults to 0.95. Confidence level for error bars
#' @param type type or error bar. Can be "confidence", "prediction", "se" or "sd"
#' @param length width of error bars (in inches) default is 0.1
#' @param mfactor mulitplication factor for error bars (when type is "se" or "sd")
#' @keywords mean errorbar
#' @export
#' @examples
#' library(agricolae)
#' data(plrv)
#' mean_errorbar(Yield~Genotype,data=plrv)

mean_errorbar<-function(formula,data,conf.level=0.95,length=0.1,type="confidence",las=2,mfactor=1,output=F,...){
  means<-aggregate(formula,data,mean,na.rm=T)
  if(type=="confidence"|type=="se"){
    se<-aggregate(formula,data,se,na.rm=T)}
  if(type=="prediction"|type=="se"){
    se<-aggregate(formula,data,sd,na.rm=T)}
  
  N<-aggregate(formula,data,N,na.rm=T)
  
  if(type=="se"|type=="sd"){t1<-mfactor}
  else{
    t1<-qt(1-(1-conf.level)/2,N[,ncol(N)])
  }
  
  y<-means[,ncol(means)]
  if(ncol(means)>2){
    names(y)<-apply(means[,-(ncol(means))],1,collapse)
  }
  else{
    names(y)<-means[,1]  
  }
  if(type=="confidence"){ylab1<-paste("Mean + ",100*conf.level,"% Confidence Interval",sep="")}
  if(type=="prediction"){ylab1<-paste("Mean + ",100*conf.level,"% Prediction Interval",sep="")}
  if(type=="sd"){ylab1<-"Mean +- Standard Deviation"}
  if(type=="se"){ylab1<-"Mean +- Standard Error"}
  
  plot(y,pch=16,ylim=c(min(means[,ncol(means)]-t1*se[,ncol(se)]),
                       max(means[,ncol(means)]+t1*se[,ncol(se)])),xaxt="n",xlab="",
       ylab=ylab1,las=las,...)
  arrows(x0=1:nrow(means),y0=means[,ncol(means)]-t1*se[,ncol(se)],y1=means[,ncol(means)]+t1*se[,ncol(se)],angle=90,code=3,length=length)
  axis(1,at = 1:length(y),labels=names(y),las=las)
  if(output==T){
    out<-data.frame(means[,-(ncol(means))],mean=y,N=N[,(ncol(N))],se=se[,(ncol(se))],upper=means[,ncol(means)]+t1*se[,ncol(se)],lower=means[,ncol(means)]-t1*se[,ncol(se)])
    return(out)
  }
}
