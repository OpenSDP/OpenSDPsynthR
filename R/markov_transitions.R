#' Create a series of binary variables based on a transition matrix
#'
#' @param n an integer representing the length of the series
#' @param tm a Transition Matrix that describes the transition matrix
#' @param burnin integer, number of values to simulate before drawing from the series
#' @param ... additional arguments to apss to \code{\link{markovchainSequence}}
#' @return a vector of length \code{n} with a random series of 0 and 1 generated
#' from the \code{tm}
#' @description Generate a
#' @details A Transition Matrix is a 2x2 matrix where:
#' \itemize{
#'  \item Element 1, 1 is the probability of moving to state 0 conditional on being in state 0
#'  \item Element 2, 1 is the probability of moving to state 0 conditional on being in state 1
#'  \item Element 1, 2 is the probability of moving to state 1 conditional on being in state 0
#'  \item Element 2, 2 is the probability of moving to state 1 conditional on being in state 1
#' }
#' @source \itemize{
#' \item \url{http://stats.stackexchange.com/questions/14175/how-to-generate-random-auto-correlated-binary-time-series-data}
#' \item \url{https://en.wikipedia.org/wiki/Examples_of_Markov_chains}
#' }
#' @importFrom markovchain markovchainSequence
#' @export
#' @examples
#' make_markov_series(10, matrix(c(0.6,0.4,0.9,0.1), nrow=2, byrow=TRUE))
make_markov_series <- function(n, tm, burnin = NULL, ...){
  stopifnot(is.matrix(tm))
  stopifnot(n > 0)
  if(any(tm > 1)){
    warning("TM elements exceed 1, adjusting by dividing by rowSums")
    tm <- tm / rowSums(tm)
  }
  mc <- new("markovchain", transitionMatrix = tm)
  if(!is.null(burnin)){
    series <- markovchainSequence(n+burnin, mc, ...)
    series <- series[burnin:(n+burnin)]
  } else{
    series <- markovchainSequence(n, mc, ...)
  }
  return(series)
}

#' Create an autocorrelated binary series
#'
#' @param n integer, length of series to generate
#' @param mean numeric between 0 and 1, proportion of cases with value 1
#' @param corr numeric between -1 and 1, how correlated should series be?
#' @description Generate a binary series that is autocorrelated using the
#' Markov method from \code{\link{make_markov_series}}
#'
#' @return A binary series
#' @export
#' @examples
#' make_binary_series(n=12,mean=0.5,corr=0.9)
#' make_binary_series(n=100,mean=0.5,corr=0.1)
make_binary_series <- function(n = 100, mean = 0.5, corr = 0){
  p01 <- corr * (1 - mean) / mean
  tm <- matrix(c(1 - p01, p01, corr, 1 - corr), nrow=2, byrow=T)
  tm <- matrix(c(0.2, 1.24, 1.3, -0.4), nrow=2, byrow=T)
  if(any(tm > 1 | tm < 0)){
    tm[1, ] <- prop.table(sqrt(tm[1, ]^2))
    tm[2, ] <- prop.table(sqrt(tm[1, ]^2))
  }
  tm <- t(apply(tm, 1, function(x)(x-min(x))/(max(x)-min(x))))
  make_markov_series(n, tm)
}


#' Identify the parameters that define a series of binary outcomes
#'
#' @param series a vector of 0 and 1 values
#' @param return a character with two options, matrix returns a transition
#' matrix, "fit" returns a \code{\link{markovchain}} object
#' @param ... additional arguments to pass to \code{\link{markovchainFit}}
#' @return Either a transition matrix or a list with parameters mean and cor
#' defining the transitions in the vector
#' @importFrom markovchain markovchainFit
#' @export
#' @examples
#' series <- make_markov_series(10, matrix(c(0.3, 0.7, 0.25, 0.75),
#'                        nrow = 2, byrow =TRUE))
#' fit_series(series)
fit_series <- function(series, return = c("matrix", "fit"), ...){
  if(missing(return)){
    return <- "matrix"
  }
  # TODO check to ensure this coerces into a true transition matrix
  # seqMat <- createSequenceMatrix(series)
  out <- markovchainFit(series,  ...)
  if(return == "matrix"){
    return(out$estimate)
  } else if(return == "fit") {
    return(out)
  }
}


