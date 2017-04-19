#' Standard OxC Interaction plot using ggplot2 so looks nicer than the base R function with minimal effort
#'
#' Plot comparing two treatments tested in multiple identical locations from long format data
#' @param options option/genotype variable making up different lines
#' @param contexts context/environment variable making up x axis
#' @param outcome vector containing numeric outcome values for y axis
#' @param sort sort the x axis from low to high or maintain pre-existing order
#' @param aggregate functionn to aggregate data (defaults to mean)
#' @keywords Interaction plot ggplot2
#' @export
#' @examples
#' library(agricolae)
#' data(plrv)
#' OxCPlot(options=plrv$Genotype,contexts=plrv$Locality,outcome=plrv$Yield)


OxCPlot<-function(options,contexts,outcome,sort=T,aggregate="mean",optionlab="",contextlab="",outcomelab="",title=""){
  #Assess Variability
  
  data<-data.frame(options=factor(options),contexts,outcome)
  
  require(ggplot2)
  require(reshape2)
  
 
  if(sort==T){
    data$contexts<-factor(data$contexts,levels=names(sort(tapply(outcome,contexts,aggregate,na.rm=T))))
  }
  
    p1<-ggplot(aes(y=outcome,x=contexts,group=options),data=data)+
      stat_summary(geom="line",aes(colour=options),fun.y = aggregate,fun.args = list(na.rm=T))
    
    if(optionlab!=""){
      p1<-p1+scale_color_discrete(name=optionlab)
    }
    if(contextlab!=""){
      p1<-p1+xlab(contextlab)
    }
    if(outcomelab!=""){
      p1<-p1+ylab(outcomelab)
    }
    else{
      p1<-p1+ylab(paste(aggregate," of ",deparse(substitute(outcome)),sep="")) 
    }
    if(title!=""){
      p1<-p1+ggtitle(title)
    }
    
  
      p1
}
