#' @importFrom stats rbinom runif
#' @importFrom utils data head tail
#' 

.onAttach = function(...) {
  if (!interactive()) return()
  msg = "Welcome to OpenSDP." # nocov
  packageStartupMessage(paste(strwrap(msg), collapse = "\n")) # nocov
}