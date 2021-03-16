#' My normal curve creates a normal distribution
#'
#' Creates a normal distribution with a given mean and standard
#' deviation. Then, it shades the area of the lower tail below a
#' given y value, as well as outputing the probability of that area
#'
#' @param mu Mean
#' @param sigma Standard deviation
#' @param a y value to calculate P(Y <= a)
#'
#' @export

myncurve = function(mu,sigma,a) {
  #normal curve
  curve(dnorm(x,mean=mu,sd=sigma),
        xlim=c(mu-3*sigma,mu+3*sigma))
  #shaded area
  xcurve=seq(mu-3*sigma, a,length=1000)
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)
  polygon(c(mu-3*sigma,xcurve,a),c(0,ycurve,0),col="red")

  #probability
  pnorm(a,mean=mu,sd=sigma)
}
