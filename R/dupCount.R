#' Counting duplicates
#'
#' @param x data frame or vector for duplicate checking
#' @importFrom plyr join count
dupcount<-function(x){
  
  #make sure x is a dataframe or can be coerced into a dataframe
  x<-data.frame(x)
  
  #calculate the frequency of each combo. (using plyr:: because the function name is used in other packages so need explicit-ness)
  counts<-plyr::count(x)
  
  #merge onto dataset. Adding a call to suppressMessages() because join() likes to tell you stuff a bit unneccesarily otherwise.
  x<-suppressMessages(plyr::join(x,counts))
  
  #return column. Minus 1 so that the number represents number of other matches (i.e. doesn't include itself); so zero for unique, 1 for 1 match etc
  return(x$freq-1)
}
