#' I like the collapse function in paste so much that I am going to make it it's own function
#' This is probably useful to nobody apart from me
#'@param x a vector to be collapse
#' @param sep collapsing character
#' @export
#' @examples x<-c("A","B","C")
#' collapse(x)


collapse<-function(x,sep="."){
  paste(x,collapse=sep)
}
