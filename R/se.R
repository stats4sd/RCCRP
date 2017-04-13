#' Standard Error
#'
#' This function calculates the naive standard error of a vector
#' @param x a numeric vector
#' @param na.rm remove missing values? (defaults to FALSE)
#' @keywords standard error se
#' @export
#' @examples
#' y<-c(1,2,3,4,NA)
#' se(y)
#' se(y,na.rm=T)



se<-function(x,na.rm=F){
  if(is.numeric(x)){
  return(sqrt(sd(x,na.rm=na.rm)/N(x,na.rm=na.rm)))
  }
  else{
    stop("Input vector is not numeric")
  }
}