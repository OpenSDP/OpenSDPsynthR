# Expand DF into grid, from SO
#
#' Expand dataframe into a complete grid
#'
#' @param ... dataframe
#' @details From SO {\url{http://stackoverflow.com/questions/11693599/alternative-to-expand-grid-for-data-frames}}
#' @return an expanded data frame
#' @export
expand_grid_df <- function(...){
  Reduce(function(...) merge(..., by=NULL), list(...))
}

##' Function to calculate age from date of birth.
##' @description his function calculates age in days, months, or years from a
##' date of birth to another arbitrary date. This returns a numeric vector in
##' the specified units.
##' @param dob a vector of class \code{Date} representing the date of birth/start date
##' @param enddate a vector of class Date representing the when the observation's
##' age is of interest, defaults to current date.
##' @param units character, which units of age should be calculated? allowed values are
##' days, months, and years
##' @param precise logical indicating whether or not to calculate with leap year
##' and leap second precision
##' @return A numeric vector of ages the same length as the dob vector
##' @source This function was developed in part from this response on the R-Help mailing list.
##' @seealso See also \code{\link{difftime}} which this function uses and mimics
##' some functionality but at higher unit levels.
##' @author Jason P. Becker
##' @export
##' @examples
##' a <- as.Date(seq(as.POSIXct('1987-05-29 018:07:00'), len=26, by="21 day"))
##' b <- as.Date(seq(as.POSIXct('2002-05-29 018:07:00'), len=26, by="21 day"))
##'
##' age <- age_calc(a, units='years')
##' age
##' age <- age_calc(a, units='months')
##' age
##' age <- age_calc(a, as.Date('2005-09-01'))
##' age
age_calc <- function (dob, enddate = Sys.Date(),
                      units = "months", precise = TRUE){
  if (!inherits(dob, "Date") |
      !inherits(enddate, "Date")) {
    stop("Both dob and enddate must be Date class objects")
  }
  if (any(enddate < dob)) {
    stop("End date must be a date after date of birth")
  }
  start <- as.POSIXlt(dob)
  end <- as.POSIXlt(enddate)
  if (precise) {
    start_is_leap <-
      ifelse(start$year %% 400 == 0,
             TRUE,
             ifelse(
               start$year %% 100 ==
                 0,
               FALSE,
               ifelse(start$year %% 4 == 0, TRUE, FALSE)
             ))
    end_is_leap <-
      ifelse(end$year %% 400 == 0,
             TRUE,
             ifelse(end$year %% 100 ==
                      0, FALSE, ifelse(end$year %%
                                         4 == 0, TRUE, FALSE)))
  }
  if (units == "days") {
    result <- difftime(end, start, units = "days")
  }
  else if (units == "months") {
    months <- sapply(mapply(
      seq,
      as.POSIXct(start),
      as.POSIXct(end),
      by = "months",
      SIMPLIFY = FALSE
    ),
    length) - 1
    if (precise) {
      month_length_end <- ifelse(end$mon == 1 & end_is_leap,
                                 29,
                                 ifelse(end$mon == 1, 28, ifelse(end$mon %in%
                                                                   c(3, 5, 8, 10), 30, 31)))
      month_length_prior <- ifelse((end$mon - 1) == 1 &
                                     start_is_leap,
                                   29,
                                   ifelse((end$mon - 1) == 1,
                                          28, ifelse((end$mon - 1) %in% c(3, 5, 8, 10),
                                                     30, 31)))
      month_frac <- ifelse(
        end$mday > start$mday,
        (end$mday -
           start$mday) / month_length_end,
        ifelse(
          end$mday <
            start$mday,
          (month_length_prior - start$mday) / month_length_prior +
            end$mday /
            month_length_end,
          0
        )
      )
      result <- months + month_frac
    }
    else {
      result <- months
    }
  }
  else if (units == "years") {
    years <- sapply(mapply(
      seq,
      as.POSIXct(start),
      as.POSIXct(end),
      by = "years",
      SIMPLIFY = FALSE
    ),
    length) - 1
    if (precise) {
      start_length <- ifelse(start_is_leap, 366, 365)
      end_length <- ifelse(end_is_leap, 366, 365)
      start_day <- ifelse(start_is_leap & start$yday >=
                            60, start$yday - 1, start$yday)
      end_day <- ifelse(end_is_leap & end$yday >= 60, end$yday -
                          1, end$yday)
      year_frac <- ifelse(
        start_day < end_day,
        (end_day -
           start_day) / end_length,
        ifelse(
          start_day > end_day,
          (start_length - start_day) /
            start_length + end_day / end_length,
          0
        )
      )
      result <- years + year_frac
    }
    else {
      result <- years
    }
  }
  else {
    stop("Unrecognized units. Please choose years, months, or days.")
  }
  return(result)
}


#' Generate positive random numbers that sum to a value
#'
#' @param N number of numbers to generate
#' @param M constraint on the sum
#' @param sd standard deviation of values
#'
#' @return a vector of numerics, length N
#' @importFrom stats rlnorm
#' @export
#' @examples
#' out <- rand_vect_cont(N = 10, M = 2, sd = 1)
#' sum(out)
#' length(out)
rand_vect_cont <- function(N, M, sd = 1) {
  vec <- rlnorm(N, M/N, sd)
  vec / sum(vec) * M
}

#' Convert auto-generated grades to better grades
#'
#' @param x a vector of grades formatted with g
#'
#' @return a character vector of grades
convert_grade <- function(x){
  x <- as.character(x)
  x[x == "g-1"] <- "PK"
  x[x == "g0"] <- "KG"
  x[x %in% paste0("g", 1:12)] <- sapply(x[x %in% paste0("g", 1:12)],
                                        function(x) gsub("g", "", x))
  x <- as.character(unlist(x))
  return(x)
}

#' Generate simulated predictions from a linear model that account for
#' model and parameter error
#'
#' @param object an object of class lm or glm
#' @param nsim number of simulations per observation to generate
#' @param newdata dataframe containing the observations to generate predictions
#' for
#' @param resid_error should the model residual error be added to predictions,
#' default is FALSE
#' @importFrom mvtnorm rmvnorm
#'
#' @return a matrix of predictions nrow(newdata) x nsim columns
#' @export
better_sim.lm <- function(object, nsim, newdata, resid_error = FALSE){
  newdata <- as.matrix(newdata)
  se <- vcov(object)
  eff <- coef(object)
  coefs <- mvtnorm::rmvnorm(nsim, mean = eff, sigma = se)
  if(ncol(newdata) == ncol(coefs)-1){
    warning("One too few variables in newdata, appending intercept to front")
    newdata <- cbind("Intercept" = 1, newdata)
  } else if(ncol(newdata) != ncol(coefs)){
    stop("Wrong dimensions in newdata")
  }
  preds <- as.matrix(newdata) %*% t(coefs)
  error <- rnorm(length(preds), mean = 0, sd = sigma(object))
  if(resid_error){
    preds <- preds + error
  }
  return(preds)
}


#' Covnert a matrix to a row-wise transition matrix
#'
#' @param matrix a matrix tof counts
#'
#' @return matrix M divided by the sum of its rows
#' @export
#'
#' @examples
#' base_mat <- structure(c(44985, 740, 781, 7640), .Dim = c(2L, 2L),
#' .Dimnames = list(c("0", "1"), c("0", "1")))
#' tm_convert(base_mat)
tm_convert <- function(matrix){
  stopifnot(class(matrix) == "matrix")
  out <- matrix / rowSums(matrix)
  return(out)
}

#' Unscale a scaled variable
#'
#' @param x numeric vector that has been scaled
#' @param mean a numeric, the mean to add to x
#' @param sd a numeric, the standardized factor to divide x by
#'
#' @return x rescaled with mean and sd specified by the user
#' @export
unscale <- function(x, mean, sd) {
  y <- (x / sd) + mean
  return(y)
}

#' Reliabily rescale numerics with missingness
#'
#' @param x a numeric vector
#'
#' @return x mean centered and divided by it's standard deviation
#' @details If \code{sd(x)} is undefined, this returns a zero
#' @export
rescale <- function(x){
  y <- (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
  y[is.na(y)] <- 0
  return(y)
}

#' Recode numeric data into plausible credit data
#'
#' @param x numeric, vector of numerics to be truncated
#' @param top integer, maximum allowable value
#' @details enforces no negative numbers and truncates data at a user specified
#' maximum
#' @return a vector, length of x, with truncated values
#' @export
recode_credits <- function(x, top = 6){
  y <- x
  y[y < 0] <- 0 #base code
  y[y > top] <- top # top code
  return(y)
}


#' Rescaling variables in groups
#'
#' @param data a dataframe containing variables you wish to rescale
#' @param var name of the variable to be rescaled
#' @param group_var character vector of the grouping terms
#' @param newvar optional character vector for the name of the new rescaled variable
#' @return data with the newvar appended
#' @export
#' @importFrom lazyeval interp
group_rescale <- function(data, var, group_var, newvar=NULL){
  if(is.null(newvar)){
    newvar <- var
  }
  varval <- lazyeval::interp(~ OpenSDP.data::rescale(z), z = as.name(var))
  data <- data %>% group_by_(group_var) %>%
    mutate_(.dots = setNames(list(varval), newvar))
  return(data)
}

#' Convert a character representation of grades to numeric
#'
#' @param grade a character vector of grades with character labels
#'
#' @return a numeric vector, length of grade, representing grade levels as numbers
#' @export
num_grade <- function(grade){
  grade[grade == "g13"] <- "13"
  grade[grade == "KG"] <- "0"
  grade[grade == "PK"] <- "-1"
  grade <- as.numeric(grade)
  return(grade)
}

#' Convert NA values to 0 in a vector
#'
#' @param x a numeric vector containing NAs
#'
#' @return a numeric vector where all NA values are 0
#' @export
zeroNA <- function(x){
  x[is.na(x)] <- 0
  return(x)
}

#' Clip a vector to be between a minimum and a maximum
#'
#' @param x a numeric vector
#' @param min numeric, a floor
#' @param max numeric, a ceiling
#'
#' @return x, truncated to be between min and max
#' @import Rcpp
#' @export
num_clip <- function(x, min, max){
  # TODO Rcpp::cppFunction('NumericVector num_clip( NumericVector x, double a, double b){
  # return clamp( a, x, b ) ;
   #  }')
  x <- num_clip_cpp(x, min, max)
  # x <- ifelse(x > max, max, x)
  # x <- ifelse(x < min, min, x)
  return(x)
}

#' Validate probability list formatting and structure
#'
#' @param list list that is to be passed as a probability list to simulation functions
#'
#' @return Logical TRUE if list is valid, error if list is not valid.
#' @export
validate_probability_list <- function(list){
  stopifnot(class(list) == "list")
  if(!"GROUPVARS" %in% names(list)){
    stop("List must contain a named element GROUPVARS that defines the variable
         in the data to assign different probability functions, e.g. Sex or Race")
  }
  if(!class(list$GROUPVARS) %in% c("character", "factor")){
    stop("List element GROUPVARS must be a character or factor which can be
         coerced to a character which defines the levels of a grouping term.")
  }
  if(!all(unlist(lapply(gifted_list[-which("GROUPVARS" == names(gifted_list))], class)) == "list")){
    stop("List elements not GROUPVARS must each be a list.")
  }
  if(!all(unlist(
    lapply(
      lapply(
        gifted_list[-which("GROUPVARS" == names(gifted_list))], names),
      function(x) x == c("f", "pars"))))){
    stop("All list elements except GROUPVARS must contain exactly two elements,
         'f' and 'pars'.")
  }
  message("List validated.")
  return(TRUE)
}



#' Validate outcome simulation list formatting and structure
#'
#' @param list list that is to be passed as a parameter list to outcome simulation functions
#'
#' @return Logical TRUE if list is valid, error if list is not valid.
#' @export
validate_sim_parameter <- function(list){
  stopifnot(class(list) == "list")
  if(any(!c("fixed", "random_var", "cov_param", "cor_vars",
           "fixed_param", "ngrps", "unbalanceRange") %in% names(list))){
    stop("List must have named elements 'fixed', 'random_var', 'cov_param',
         'cor_vars', 'fixed_param', 'ngrps', and 'unbalanceRange'")
  }
  if(class(list$fixed) != "formula"){
    stop("Fixed must be a formula object defining the fixed component of the
         outcome formula.")
  }
  if(attr(terms(list$fixed), "response") > 0){
    stop("Formula must not have a response or LHS variable defined.")
  }
  K <- length(attr(terms(list$fixed), "term.labels"))
  if(class(list$random_var) != "numeric"){
    stop("Random variance must be expressed as a numeric value. Consider a value
         in the range of 0.01 to 0.2")
  }
  if(length(list$random_var) > 1){
    stop("Random variance element of the list can only be length 1.")
  }
  if(class(list$cov_param) != "list"){
    stop("cov_param element must be a list that defines the parameters of the
         covariate simulation.")
  }
  if(!all(c("dist_fun", "var_type", "opts") %in% names(list$cov_param))){
    stop("cov_param list must have three elements, dist_fun, var_type, and opts")
  }
  if(length(list$cov_param$dist_fun) != K){
    stop("cov_param$dist_fun must have the same length as the terms in list$fixed")
  }
  if(length(list$cor_vars) != (K^2 - K)/2){
    cor_elements <- (K^2 - K) /2
    stop("Correlation vector cor_vars must specify ", cor_elements, " correlation
         matrix elements.")
  }
  if(length(list$fixed_param) != K+1){
    stop("List element fixed_param must define beta coefficient values for ", K+1,
         " parameters specified in the fixed formula and one intercept.")
  }
  if(class(list$fixed_param) != "numeric"){
    stop("Please define fixed parameters as numeric.")
  }
  if(length(list$ngrps) != 1){
    stop("Element ngrps must be length 1 numeric or integer")
  }
  if(!class(list$ngrps) %in% c("numeric", "integer")){
    stop("Element ngrps must be an integer or numeric")
  }
  if(length(list$unbalanceRange) != 2){
    stop("Element unbalanceRange must be length 2, numeric or integer")
  }
  if(!class(list$unbalanceRange) %in% c("numeric", "integer")){
    stop("unbalanceRange must be numeric or integer")
  }
  message("Simulation parameters validated.")
  return(TRUE)
}


#' Simple error assertions
#'
#' @param expr logical expression
#' @param error character, error message
#'
#' @return An error message, or nothing
assert <- function (expr, error) {
  if (! expr) stop(error, call. = FALSE)
}
