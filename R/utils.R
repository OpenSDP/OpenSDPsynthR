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
#' @importFrom mvtnorm rmvnorm
#'
#' @return a matrix of predictions nrow(newdata) x nsim columns
#' @export
better_sim.lm <- function(object, nsim, newdata){
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

