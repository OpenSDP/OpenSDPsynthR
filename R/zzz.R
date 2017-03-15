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
  reduce(l, `+`) %>% list()
}
