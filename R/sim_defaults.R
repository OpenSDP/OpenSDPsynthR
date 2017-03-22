## Break into discrete functions

## Calculate ages, toss out non-sensical records
## add longitudinal indicators
##

#' Generate student-level attributes
#'
#' @param n integer, number of students to simulate
#' @param seed integer, random seed to use
#' @param control a list, defined by \code{\link{sim_control}}
#' @import dplyr
#' @importFrom wakefield sex
#' @importFrom wakefield dob
#' @importFrom wakefield id
#' @importFrom wakefield race
#' @return a data.frame
#' @export
gen_students <- function(n, seed, control){
  set.seed(seed)
  if(is.null(control$race_groups)){ # use is.null because sim_control passes null values
    control$race_groups <- xwalk$CEDS_name[xwalk$category == "Demographic"]
    control$race_groups <- control$race_groups[!control$race_groups %in% c("Sex", "","Birthdate")]
  }
  if(is.null(control$race_prob)){
    control$race_prob <- c(0.637, 0.047, 0.007, 0.122, 0.163, 0.021, 0.0015)
  }
  if(!is.null(control$minyear)){
    tmp <- paste0(minyear, "-01-01")
    start <- as.integer(Sys.Date() - as.Date(tmp))
    start <- start + (365 * 4.25)
  }
  if(is.null(control$n_cohorts)){
    K <- 365L * 8L # Need to test these integers
  } else{
    K <- 365L * n_cohorts
  }
  demog_master <- data.frame(
    sid = wakefield::id(n, random = TRUE),
    "Sex" = wakefield::sex(n),
    "Birthdate" = wakefield::dob(n, start = Sys.Date() - start,
                           k = K, by = "1 days"),
    "Race" = wakefield::race(n, x = control$race_groups, prob = control$race_prob)
  )
  demog_master$Race <- factor(demog_master$Race)
  demog_master %<>% make_inds("Race")
  # What does this line do?
  demog_master %<>% mutate_at(5:ncol(demog_master),
                              funs(recode(., `0` = "No", `1` = "Yes")))
  if(is.null(control$ses_list)){
    control$ses_list <- OpenSDP.data:::ses_list
  }
  demog_master <- as.data.frame(demog_master)
  demog_master <- cond_prob(demog_master, factor = "Race",
                            newvar = "ses", prob_list = control$ses_list)
  return(demog_master)

}


#' Grand simulation
#' @rdname popsim_control
#' @param n integer, number of students to simulate
#' @param seed integer, random seed to use
#' @param control a list, defined by \code{\link{sim_control}}
#' @return a list with simulated data
#' @importFrom lubridate year
#' @import dplyr
#' @export
#' @examples
#' out <- popsim_control(20, seed = 213)
popsim_control <- function(n, seed, control = sim_control()){
  ## Generate student-year data
  demog_master <- gen_students(n = n, seed = seed, control = control)
  stu_year <- gen_student_years(data = demog_master, control = control)
  # stu_year$age <- age_calc(dob = stu_year$DOB,
  #                          enddate = as.Date(paste0(stu_year$year, "-09-21")),
  #                          units = "years", precise = TRUE)
  return(list(demog_master = demog_master, stu_year = stu_year))
}


#' Generate annual student observations
#'
#' @param data students to generate annual data for
#' @param control a list, defined by \code{\link{sim_control}}
#' @return a data.frame
#' @export
gen_student_years <- function(data, control){
  stu_year <- vector(mode = "list", nrow(data))
  stopifnot(any(c("ID", "id", "sid") %in% names(data)))
  if(is.null(control$minyear)){
    control$minyear <- 1997
  }
  if(is.null(control$maxyear)){
    control$maxyear <- 2017
  }
  idvar <- names(data)[which(names(data) %in% c("ID", "id", "sid"))]
  # Make a list of dataframes, one for each student, for each year
  for(i in 1:nrow(data)){
    tmp <- expand_grid_df(data[i, idvar],
                          data.frame(year = control$minyear:control$maxyear))
    # tmp$year <- lubridate::year(tmp$DOB + (tmp$year + 4) * 365)
    # tmp$year - lubridate::year(tmp$DOB)
    stu_year[[i]] <- tmp; rm(tmp)
  }
  stu_year <- bind_rows(stu_year) %>% as.data.frame()
  names(stu_year) <- c(idvar, "year")
  return(stu_year)
}


#' Set control parameters for simulated data
#' @param race_groups vector of labels for race groups
#' @param race_prob vector of numerics, same length as \code{race_groups}
#' @param ses_list a probability list
#' @param minyear an integer
#' @param maxyear an integer
#'
#' @return a named list
#' @export
sim_control <- function(race_groups=NULL, race_prob=NULL,
                        ses_list=NULL, minyear=1997, maxyear=2017,
                        n_cohorts = NULL){


  structure(namedList(
                 race_groups,
                 race_prob,
                 ses_list,
                 minyear,
                 maxyear))

}


#' Create a named list
#'
#' @param ... arguments to pass to the list
#'
#' @return a named list
namedList <- function(...) {
  L <- list(...)
  snm <- sapply(substitute(list(...)), deparse)[-1]
  if (is.null(nm <- names(L))) nm <- snm
  if (any(nonames <- nm == "")) nm[nonames] <- snm[nonames]
  setNames(L,nm)
}

