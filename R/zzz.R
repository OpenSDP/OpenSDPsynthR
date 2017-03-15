#' @importFrom stats rbinom runif
#' @importFrom utils data head tail
#' @importFrom methods new

.onAttach = function(...) {
  if (!interactive()) return()
  msg = "Welcome to OpenSDP." # nocov
  packageStartupMessage(paste(strwrap(msg), collapse = "\n")) # nocov
}


#' Sum matrix elements in a list
#'
#' @param l a list
#'
#' @return a list of summed matrix elements
#' @importFrom magrittr %>%
m_sum <- function(l) {
  reduce(l, `+`) %>% lisst()
}

#' Tidy a two-state markov sequence for output
#'
#' @param seq a vector of sequence elements, with only two states
#' @param states a vector of length two naming both possible states
#'
#' @return a data.frame
#' @importFrom markovchain createSequenceMatrix
#' @export
#' @examples
#' tidy_sequence(seq = c("Yes", "No", "No", "No", "Yes", "Yes"),
#'              states = c("Yes", "No"))
tidy_sequence <- function(seq, states){
  stopifnot(length(states) == 2)
  tmp <- markovchain::createSequenceMatrix(seq, possibleStates = states)
  newNames <- outer(dimnames(tmp)[[1]], dimnames(tmp)[[2]], paste, sep = "-")
  dim(newNames) <- NULL
  dim(tmp) <- c(1, 4)
  tmp <- as.data.frame(tmp)
  colnames(tmp) <- newNames
  return(tmp)
}
