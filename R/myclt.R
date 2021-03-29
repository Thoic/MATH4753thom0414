#' My clt creates a histogram of sums
#'
#' Creates a histogram of sums from the random variable
#' uniform distribution for a given a and b,
#' with sample size n over iter iterations
#'
#' @param n sample size
#' @param iter iterations
#' @param a minimum value of uniform
#' @param b maximum value of uniform
#'
#' @export
myclt=function(n,iter,a=0,b=5){
y=runif(n*iter,a,b)
data=matrix(y,nr=n,nc=iter,byrow=TRUE)
sm=apply(data,2,sum)
h=hist(sm,plot=FALSE)
hist(sm,col=rainbow(length(h$mids)),freq=FALSE,main="Distribution of the sum of uniforms")
curve(dnorm(x,mean=n*(a+b)/2,sd=sqrt(n*(b-a)^2/12)),add=TRUE,lwd=2,col="Blue")
sm
}
