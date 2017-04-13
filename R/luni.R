#' Number of Unique Values
#'
#' This function calculates the sample size with option to include or exclude missing values for compatibility with other simple R functions
#' @param x any R object that can be coerced into a vector
#' @keywords number of unique values
#' @export
#' @examples
#' y<-c("A","A","B","C","D",NA,"A")
#' luni(y)
#' luni(y,na.rm=T)
#'luni(y,na.rm=F,printvals=T)

luni<-function(x,na.rm=T,printvals=F){
  if(na.rm==T){
    if(printvals==F){return(length(unique(as.character(x[is.na(x)==F]))))}
    if(printvals==T){return(list(luni=length(unique(as.character(x[is.na(x)==F]))),tab=table(x,useNA="no")))}
    
  }
  else{
    if(printvals==F){return(length(unique(as.character(x))))}
    if(printvals==T){return(list(luni=length(unique(as.character(x))),tab=table(x,useNA="ifany")))}
    
  }
}

 