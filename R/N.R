#' Sample Size
#'
#' This function calculates the sample size with option to include or exclude missing values for compatibility with other simple R functions
#' @param x any R object that can be coerced into a vector
#' @keywords length N
#' @export
#' @examples
#' y<-c(1,2,3,4,NA)
#' N(y)
#' N(y,na.rm=T)

N<-function(x,na.rm=F){
  x<-as.vector(x)
  if(na.rm==T){
    y<-length(x[is.na(x)==F])
  }else{
    y<-length(x[is.na(x)==F])
  }
  return(y)
}
