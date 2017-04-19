#' Cultivar Superiority
#'
#' Equivalent to GESTABILITY function in Genstat
#' Calculates stability coefficients for genotype-by-environment data (R.W. Payne).
#' @param genotypes vector containing genotype (or option) ID
#' @param environments vector containing environment (or context) ID
#' @param outcome vector containing numeric outcome values
#' @param tolerance tolerance level for convergence
#' @param cex.label size of labels on plot
#' @param ... other arguments to plot()
#' @keywords Cultivar Superiority
#' @export
#' @examples
#' library(agricolae)
#' data(plrv)
#' superiority(plrv$Genotype,plrv$Locality,plrv$Yield)

superiority<-function(genotypes,environments,outcome,cex.label=0.8,...){
  require(doBy)
  
  dat<-droplevels(data.frame(outcome=outcome,gen=genotypes,env=environments))
  dat<-na.omit(dat)
  if(nrow(subset(data.frame(table(dat$gen,dat$env)),Freq==0))>0){
    
    stop(paste("Genotypes",paste(unique(subset(data.frame(table(dat$gen,dat$env)),Freq==0)$Var1),collapse="; "),"not present in all environments"))
  }
  dat$gen<-as.factor(dat$gen)
  dat$env<-as.factor(dat$env)
  n.gen <- nlevels(dat$gen)
  n.env <- nlevels(dat$env)
  
  
  mean_gen<-summaryBy(outcome~gen,data=dat,FUN=c(mean,se),na.rm=T)
  colnames(mean_gen)[2]<-"gen.mean"
  colnames(mean_gen)[3]<-"gen.se"
  mean_env<-summaryBy(outcome~env,data=dat,FUN=mean,na.rm=T)
  colnames(mean_env)[2]<-"env.mean"
  mean_int<-summaryBy(outcome~gen+env,data=dat,FUN=mean,na.rm=T)
  colnames(mean_int)[3]<-"gxe.mean"
  
  mj<-summaryBy(gxe.mean~env,data=mean_int,FUN=max,na.rm=T)
  
  mean_int2<-merge(mean_int,mj)
  mean_int2$Pij<-((mean_int2$gxe.mean-mean_int2$gxe.mean.max)**2)/(2*n.env)
  
  Pi<-summaryBy(Pij~gen,data=mean_int2,FUN=sum,na.rm=T)
  P<-merge(Pi,mean_gen)
  colnames(P)[2]<-"Cultivar.Superiority"
  plot(P$gen.mean,P$Cultivar.Superiority,ylab="Cultivar Superiority",xlab="Genotype Mean",pch="",...)
  text(P$gen.mean,P$Cultivar.Superiority,P$gen,cex=cex.label)
  P$Rank<-rank(P$Cultivar.Superiority)
  return(P)
}