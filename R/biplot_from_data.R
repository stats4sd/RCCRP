#' Produce GGE Biplot From Raw Data
#'
#' Convenience function to transform raw data into format required for GGEBiplotGUI package
#' @param genotypes vector containing genotype (or option) ID
#' @param environments vector containing environment (or context) ID
#' @param outcome vector containing numeric outcome values
#' @param all.genotypes when not all combinations of GxE exist force biplot to contain all genotypes (defaults to FALSE)
#' @param all.environments when not all combinations of GxE exist force biplot to contain all environments (defaults to TRUE)
#' @keywords biplot GGEBiplot wrapper 
#' @export
#' @examples
#' library(agricolae)
#' data(plrv)
#' biplot_from_data(plrv$Genotype,plrv$Locality,plrv$Yield)


biplot_from_data<-function(genotypes,environments,outcome,all.genotypes=F,all.environments=T){
  require(reshape2)
  require(GGEBiplotGUI)
  x<-data.frame(genotypes,environments,outcome)
  
  y<-dcast(genotypes~environments,data=x,value.var="outcome",fun.aggregate = mean,na.rm=T)

  rownames(y)<-y$genotypes
  y<-y[,-1]
  colnames(y)<-levels(as.factor(as.character(environments)))
  
  if(sum(is.na(y))>0){
    if(all.genotypes==T&all.environments==T){
      print(y)
      stop("Missing values in GxE Table - Not all genotypes planted in all environments")
    }
    if(all.genotypes==F&all.environments==T){
      miss_Gen<-paste(rownames(y)[apply(y,1,FUN=function(x)sum(is.na(x)))>0],collapse=" , ")
      y<-na.omit(y)
      warning(paste("Genotypes",miss_Gen,"not planted in every environment and have been removed from biplot"))
    }
    if(all.genotypes==T&all.environments==F){
      miss_Env<-paste(colnames(y)[apply(y,2,FUN=function(x)sum(is.na(x)))>0],collapse=" , ")
      y<-y[,apply(y,2,FUN=function(x)sum(is.na(x)))==0]
      warning(paste("Environments",miss_Env,"do not contain all genotypes and have been removed from biplot"))
    }
    
  }
  
  GGEBiplot(y)
  return(y)
}