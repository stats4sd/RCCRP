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

riskplot<-function(y,splitvar=NULL,approx=T,v=0,same=T,splitlab="",...){
  sort1<-order(y)
  y<-y[sort1]
  if(class(splitvar)=="NULL"){
    plot.stepfun(y,ylab="Cumulative Percentage",
                 xlim=c(min(y,na.rm=T),max(y,na.rm=T)),col=2,lwd=2,lty=2,yaxt="n",...)
    grid()
    axis(2,at=seq(0,1,by=0.2),labels=seq(0,100,by=20),las=2)
    abline(v=v)
    plot.stepfun(y,col=2,add=T,lwd=2,...)
    if(approx==T){
      lines(y, pnorm(y,mean=mean(y,na.rm=T),sd=sd(y,na.rm=T)),col=2)
    }
  }
  if(class(splitvar)!="NULL"&same==T){
    splitvar<-splitvar[sort1]
    levs<-levels(as.factor(as.character(splitvar)))
    n<-length(levs)
    plot.stepfun(y[splitvar==levs[1]],ylab="Probability",
                 xlim=c(min(y,na.rm=T),max(y,na.rm=T)),col=2,lty=2,lwd=2,...)
    grid()
    abline(v=v)
    for(i in 1:n){
      yi<-y[splitvar==levs[i]]
      plot.stepfun(yi,col=i+1,add=T,lwd=2,lty=2,...)
      if(approx==T){
        lines(yi, pnorm(yi,mean=mean(yi,na.rm=T),sd=sd(yi,na.rm=T)),col=i+1)
      }
    }
    legend("bottomright",lwd=2,col=2:(2+n),legend=levs,title=splitlab)
  }
  if(class(splitvar)!="NULL"&same==F){
    splitvar<-splitvar[sort1]
    levs<-levels(as.factor(as.character(splitvar)))
    n<-length(levs)
    for(i in 1:n){
      yi<-y[splitvar==levs[i]]
      plot.stepfun(yi,main=levs[i],ylab="Cumulative Percentage",
                   xlim=c(min(y,na.rm=T),max(y,na.rm=T)),col=2,lwd=2,yaxt="n",...)
      grid()
      axis(2,at=seq(0,1,by=0.2),labels=seq(0,100,by=20),las=2)
      abline(v=v)
      plot.stepfun(yi,col=2,add=T,lwd=2,...)
      if(approx==T){
        lines(yi, pnorm(yi,mean=mean(yi,na.rm=T),sd=sd(yi,na.rm=T)),col=2)
      }
    }
  }
}
