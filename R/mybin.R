#' My function to create a barplot of a binomial distribution
#'
#' Creates a barplot showing the number of sucesses after a given number of iterations
#'
#' @param iter The number of iterations for the sample to run
#' @param n The number of samples to take
#' @param p The probability of success
#'
#' @importFrom grDevices rainbow
#' @importFrom graphics barplot
#'
#' @export
mybin = function(iter=1000, n=10, p=0.5) {
  # make a matrix to hold the samples
  #initially filled with NA's
  sam.mat=matrix(NA, nr=n, nc=iter, byrow=TRUE)

  #Make a vector to hold the number of successes in each trial
  succ=c()

  for( i in 1:iter){
    #Fill each column with a new sample
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))

    #Calculate a statistic from the sample (this case it is the sum)
    succ[i]=sum(sam.mat[,i])
  }

  #Make a table of successes
  succ.tab=table(factor(succ,levels=0:n))

  #Make a barplot of the proportions
  iter.lab = paste0("iter = ", iter)
  n.lab = paste0("n = ", n)
  p.lab = paste0("p = ", p)
  lab = paste(iter.lab, n.lab, p.lab, sep = ", ")
  barplot(succ.tab/(iter), col=rainbow(n+1), main="Binomial simulation", sub = lab, xlab = "Number of successes")
  succ.tab/iter
}
