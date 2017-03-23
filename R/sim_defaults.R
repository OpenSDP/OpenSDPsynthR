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
#' @importFrom wakefield race
#' @return a data.frame
#' @export
gen_students <- function(n, seed, control = sim_control()){
  set.seed(seed)
  if(is.null(control$race_groups)){ # use is.null because sim_control passes null values
    control$race_groups <- xwalk$CEDS_name[xwalk$category == "Demographic"]
    control$race_groups <- control$race_groups[!control$race_groups %in% c("Sex", "","Birthdate")]
  }
  if(is.null(control$race_prob)){
    control$race_prob <- c(0.637, 0.047, 0.007, 0.122, 0.163, 0.021, 0.0015)
  }
  if(!is.null(control$minyear)){
    tmp <- paste0(control$minyear, "-01-01")
    start <- as.integer(Sys.Date() - as.Date(tmp))
    start <- start + (365 * 6) # trying to not generate PK data
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
  demog_master <- as.data.frame(demog_master)
  # Do not need to be warned about NAs in binomial
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
  message("Preparing student identities for ", n, " students...")
  suppressMessages({
    demog_master <- gen_students(n = n, seed = seed, control = control)
  })
  message("Creating annual enrollment for ", n, " students...")
  suppressMessages({
    stu_year <- gen_student_years(data = demog_master, control = control)
  })
  idvar <- names(demog_master)[which(names(demog_master) %in% c("ID", "id", "sid"))]
  # Get first observed year for student
  stu_first <- stu_year %>% group_by_(idvar) %>%
    mutate(flag = if_else(age == min(age), 1, 0)) %>%
    filter(flag == 1) %>% select(-flag) %>% as.data.frame() %>%
    select_(idvar, "year", "age")
  stu_first <- inner_join(stu_first, demog_master[, c(idvar, "Race")],
                          by = idvar)
  stu_first$age <- round(stu_first$age, 0)
  #stu_year <- left_join(stu_year, stu_first[, c(1, 6)])
  # Needs to become flexible to multipe inputs and outputs
  # Needs to avoid hardcoding Race transformations, CEDS Xwalk should take
  # place outside of this function
  # This should also be controlled somehow by control eventually
  message("Assigning ", n, " students to initial ELL status...")
  stu_first$ell <- gen_initial_status(stu_first, baseline = "ell")
  message("Assigning ", n, " students to initial SES status...")
  stu_first$ses <- gen_initial_status(stu_first, baseline = "ses")
  message("Organizing status variables for you...")
  stu_year <- left_join(stu_year, stu_first[, c(idvar, "ell")], by = idvar)
  demog_master <- left_join(demog_master, stu_first[, c(idvar, "ses")], by = idvar)
  rm(stu_first)
  message("Assigning ", n, " students longitudinal status trajectories...")
  cond_vars <- get_sim_groupvars(control)
  stu_year <- left_join(stu_year, demog_master[, c(idvar, cond_vars)])
  stu_year <- gen_annual_status(stu_year, control = control)
  stu_year <- stu_year %>% select_(idvar, "year", "age", "ell", "iep", "gifted")
  message("Sorting your records")
  stu_year <- stu_year %>% arrange_(idvar, "year")
  message("Success! Returning you student and student-year data in a list.")
  return(list(demog_master = demog_master, stu_year = stu_year))
}

#' Generate initial studen status indicators
#'
#' @param data that includes the pre-requsites for generating each status
#' @param baseline character, name of a baseline status to calculate
#'
#' @return the data with status variables appended
#' @export
gen_initial_status <- function(data, baseline){
  bl_data <- get_baseline(baseline)
  data$race <- map_CEDS(data$Race)
  # Move CEDS Xwalk out of this function eventually
  stopifnot(all(bl_data$keys %in% names(data)))
  # Assign baseline creates a new vector, so assign it
  out <- assign_baseline(baseline = baseline, data = data)
  # Recode it
  out <- ifelse(out == 1, "Yes", "No")
  return(out)
}

#' Generate annual status trajectories per student
#'
#' @param data student-year data
#' @param control control list
#'
#' @return the \code{data} object, with additional variables appended
#' @export
gen_annual_status <- function(data, control = sim_control()){
  reqdVars <- get_sim_groupvars(control)
  stopifnot(all(reqdVars %in% names(data)))
  idvar <- names(data)[which(names(data) %in% c("ID", "id", "sid"))]
  data <- data %>% group_by_(idvar) %>% arrange(year) %>%
    mutate(iep = markov_cond_list(Sex[1], n = n(), control$iep_list),
           gifted = markov_cond_list(Sex[1], n = n(), control$gifted_list),
           ell = markov_cond_list("ALL", n = n() - 1, control$ell_list,
                                  t0 = ell[1], include.t0 = TRUE))
  return(data)
}


#' Generate annual student observations
#'
#' @param data students to generate annual data for
#' @param control a list, defined by \code{\link{sim_control}}
#' @importFrom lubridate year
#' @importFrom magrittr %<>%
#' @return a data.frame
#' @export
gen_student_years <- function(data, control=sim_control()){
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
    stu_year[[i]] <- tmp; rm(tmp)
  }
  stu_year <- bind_rows(stu_year) %>% as.data.frame()
  names(stu_year) <- c(idvar, "year")
  bdvar <- names(data)[which(names(data) %in% c("DOB", "dob", "Birthdate"))]
  stu_year <- left_join(stu_year, data[, c(idvar, bdvar)])
  # Drop rows that occur before the birthdate
  stu_year %<>% filter(stu_year$year > lubridate::year(stu_year[, bdvar]))
  stu_year$age <- age_calc(dob = stu_year[, bdvar],
                          enddate = as.Date(paste0(stu_year$year, "-09-21")),
                          units = "years", precise = TRUE)
  # Cut off ages before X
  stu_year %<>% filter(stu_year$age >= 4)
  return(stu_year)
}


#' Set control parameters for simulated data
#' @param race_groups vector of labels for race groups
#' @param race_prob vector of numerics, same length as \code{race_groups}
#' @param minyear an integer
#' @param maxyear an integer
#' @param ses_list a probability list
#' @param iep_list a probability list
#' @param ell_list a probability list
#' @param gifted_list a probability list
#' @param n_cohorts number of cohorts to produce
#' @return a named list
#' @export
sim_control <- function(race_groups=NULL, race_prob=NULL,
                        ses_list=NULL, minyear=1997, maxyear=2017,
                        n_cohorts = NULL, gifted_list=NULL, iep_list=NULL,
                        ell_list=NULL){

  # temporarily hardcoding these values here for testing
  tm_gifted_f <- matrix(c(500, 1, 2, 500), nrow = 2, byrow = TRUE,
                        dimnames = list(c("Yes", "No"), c("Yes", "No")))
  tm_gifted_m <- tm_gifted_f
  tm_gifted_m[1, 1] <- tm_gifted_m[1, 1] + 25
  tm_gifted_m <- tm_gifted_m / rowSums(tm_gifted_m)
  tm_gifted_f <- tm_gifted_f / rowSums(tm_gifted_f)

  gifted_list <- list("Male" = list(f = make_markov_series,
                             pars = list(tm = tm_gifted_m,
                              # Use quote so for each call in the loop sample is redrawn
                      t0 = quote(sample(c("Yes", "No"), 1, prob = c(10, 90))))),
                      "Female" = list(f = make_markov_series,
                          pars = list(tm = tm_gifted_f,
                       t0 = quote(sample(c("Yes", "No"), 1, prob = c(8, 92))))),
                      "GROUPVARS" = c("Sex"))

  # IEP
  tm_iep_f <- matrix(c(250, 50, 150, 900), nrow = 2, byrow = TRUE,
                     dimnames = list(c("Yes", "No"), c("Yes", "No")))
  tm_iep_m <- tm_iep_f
  tm_iep_m[, 1] <- tm_iep_m[, 1] + 50
  tm_iep_m <- tm_iep_m / rowSums(tm_iep_m)
  tm_iep_f <- tm_iep_f / rowSums(tm_iep_f)

    iep_list <- list("Male" = list(f = make_markov_series,
               pars = list(tm = tm_iep_m,
                     t0 = quote(sample(c("Yes", "No"), 1, prob = c(20, 80))))),
                   "Female" = list(f = make_markov_series,
                     pars = list(tm = tm_iep_f,
                 t0 = quote(sample(c("Yes", "No"), 1, prob = c(16, 84))))),
               "GROUPVARS" = c("Sex"))

    tm <- matrix(c(800, 20, 5, 800), nrow = 2, byrow = TRUE,
                 dimnames = list(c("Yes", "No"), c("Yes", "No")))
    tm <- tm / rowSums(tm)
    ell_list <- list("ALL" = list(f = make_markov_series,
                                  pars = list(tm = tm)),
                     "GROUPVARS" = c("ell"))
  structure(namedList(
                 race_groups,
                 race_prob,
                 minyear,
                 maxyear,
                 gifted_list,
                 iep_list,
                 ses_list,
                 ell_list))

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


get_sim_groupvars <- function(control = sim_control()){
  # consider
  #https://stat.ethz.ch/R-manual/R-devel/library/base/html/rapply.html
  out <- c(control$iep_list$GROUPVARS,
           control$gifted_list$GROUPVARS,
           control$ses_list$GROUPVARS)
  unique(out)
}


