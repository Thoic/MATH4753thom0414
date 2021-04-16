#' My confidence interval
#'
#' Creates a 95% interval for mu from a single sample
#'
#' @param x sample
#'
#' @export
myci<-function(x) {
  mean(x)+c(-1,1)*qt(1-0.05/2,length(x)-1)*sd(x)/sqrt(length(x))
}
