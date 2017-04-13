## Break into discrete functions

## Calculate ages, toss out non-sensical records
## add longitudinal indicators
##

#' Generate student-level attributes
#'
#' @param nstu integer, number of students to simulate
#' @param seed integer, random seed to use
#' @param control a list, defined by \code{\link{sim_control}}
#' @import dplyr
#' @importFrom wakefield sex
#' @importFrom wakefield dob
#' @importFrom wakefield race
#' @return a data.frame
#' @export
gen_students <- function(nstu, seed, control = sim_control()){
  set.seed(seed)
  if(is.null(control$race_groups)){
    # use is.null because sim_control passes null values
    control$race_groups <- xwalk$CEDS_name[xwalk$category == "Demographic"]
    control$race_groups <- control$race_groups[!control$race_groups %in%
                                                 c("Sex", "","Birthdate")]
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
    K <- 365L * control$n_cohorts
  }
  demog_master <- data.frame(
    sid = wakefield::id(nstu, random = TRUE),
    "Sex" = wakefield::sex(nstu),
    "Birthdate" = wakefield::dob(nstu, start = Sys.Date() - start,
                           k = K, by = "1 days"),
    "Race" = wakefield::race(nstu, x = control$race_groups, prob = control$race_prob)
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
#' @rdname simpop
#' @param nstu integer, number of students to simulate
#' @param seed integer, random seed to use
#' @param control a list, defined by \code{\link{sim_control}}
#' @return a list with simulated data
#' @importFrom lubridate year
#' @import dplyr
#' @export
#' @examples
#' out <- simpop(nstu = 20, seed = 213)
simpop <- function(nstu, seed, control = sim_control()){
  ## Generate student-year data
  message("Preparing student identities for ", nstu, " students...")
  suppressMessages({
    demog_master <- gen_students(nstu = nstu, seed = seed, control = control)
  })
  message("Creating annual enrollment for ", nstu, " students...")
  suppressMessages({
    stu_year <- gen_student_years(data = demog_master, control = control)
  })
  idvar <- names(demog_master)[which(names(demog_master) %in%
                                       c("ID", "id", "sid"))]
  # Get first observed year for student
  stu_first <- stu_year %>% group_by_(idvar) %>%
    mutate(flag = if_else(age == min(age), 1, 0)) %>%
    filter(flag == 1) %>% select(-flag) %>% as.data.frame() %>%
    select_(idvar, "year", "age")
  stu_first <- inner_join(stu_first, demog_master[, c(idvar, "Race")],
                          by = idvar)
  stu_first$age <- round(stu_first$age, 0)
  message("Assigning ", nstu, " students to initial FRPL, IEP, and ELL status")
  stu_first <- assign_baseline(baseline = "program", stu_first)
  message("Assigning initial grade levels...")
  stu_first <- assign_baseline("grade", stu_first)
  message("Organizing status variables for you...")
  stu_year <- left_join(stu_year, stu_first[, c(idvar, "ell", "iep", "frpl", "grade")],
                        by = idvar)
  rm(stu_first)
  message("Assigning ", nstu, " students longitudinal status trajectories...")
  cond_vars <- OpenSDP.data:::get_sim_groupvars(control)
  stu_year <- left_join(stu_year, demog_master[, c(idvar, cond_vars)],
                        by = idvar)
  stu_year <- gen_annual_status(stu_year, control = control)
  # Create longitudinal ell and ses here
  stu_year <- stu_year %>%
    select_(idvar, "year", "age", "grade", "frpl", "ell", "iep", "gifted")
  message("Sorting your records")
  stu_year <- stu_year %>% arrange_(idvar, "year")
  message("Cleaning up...")
  stu_year$age <- round(stu_year$age, 0)
  message("Creating ", control$nschls, " schools for you...")
  school <- gen_schools(n = control$nschls, mean = control$school_means,
                        sigma = control$school_cov_mat,
                        names = control$school_names)
  message("Assigning ", nrow(stu_year), " student-school enrollment spells...")
  stu_year <- assign_schools(student = stu_year, schools = school)
  message("Simulating assessment table... be patient...")
  assess <- left_join(stu_year, demog_master[, 1:4])
  assess$male <- ifelse(assess$Sex == "Male", 1, 0)
  zz <- gen_assess(data = assess, control = control)
  assess <- bind_cols(assess[, c(idvar, "schid", "year")], zz)
  assess <- left_join(assess, stu_year[, c(idvar, "schid", "year", "grade")])
  assess <- assess[, c(idvar, "schid", "year", "grade", "math_ss", "rdg_ss")]
  rm(zz)
  message("Simulating high school outcomes... be patient...")
  g12_cohort <- stu_year[stu_year$grade == "12", ]
  g12_cohort <- na.omit(g12_cohort)
  g12_cohort <- left_join(g12_cohort, demog_master[, 1:4], by = idvar)
  g12_cohort$male <- ifelse(g12_cohort$Sex == "Male", 1, 0)
  hs_outcomes <- assign_hs_outcomes(g12_cohort, control = control)
  message("Simulating annual high school outcomes... be patient...")
  hs_annual <- gen_hs_annual(hs_outcomes)
  # TODO: Fix hardcoding of postsec
  nsc_postsec <- gen_nsc(n = 35, names = sim_control()$postsec_names)
  message("Success! Returning you student and student-year data in a list.")
  return(list(demog_master = demog_master, stu_year = stu_year,
              schools = school, assessment = assess, hs_outcomes = hs_outcomes,
              hs_annual = hs_annual, nsc = nsc_postsec))
}

#' Generate initial student status indicators
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
  reqdVars <- c(reqdVars, c("iep", "ell", "frpl", "grade"))
  stopifnot(all(reqdVars %in% names(data)))
  idvar <- names(data)[which(names(data) %in% c("ID", "id", "sid"))]
  data <- data %>% group_by_(idvar) %>% arrange(year) %>%
    mutate(iep = markov_cond_list(Sex[1], n = n()-1, control$iep_list,
                                  t0 = iep[1], include.t0 = TRUE),
           gifted = markov_cond_list(Sex[1], n = n(), control$gifted_list),
           ell = markov_cond_list("ALL", n = n() - 1, control$ell_list,
                                  t0 = ell[1], include.t0 = TRUE),
           frpl = markov_cond_list(Race[1], n = n()-1, control$ses_list,
                                  t0 = frpl[1], include.t0 = TRUE),
           grade = markov_cond_list("ALL", n = n() - 1, control$grade_levels,
                                  t0 = grade[1], include.t0 = TRUE))
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
  stu_year <- left_join(stu_year, data[, c(idvar, bdvar)], by = idvar)
  # Drop rows that occur before the birthdate
  stu_year %<>% filter(stu_year$year > lubridate::year(stu_year[, bdvar]))
  stu_year$age <- age_calc(dob = stu_year[, bdvar],
                          enddate = as.Date(paste0(stu_year$year, "-09-21")),
                          units = "years", precise = TRUE)
  # Cut off ages before X
  stu_year %<>% filter(stu_year$age >= 4)
  return(stu_year)
}




get_sim_groupvars <- function(control = sim_control()){
  # consider
  #https://stat.ethz.ch/R-manual/R-devel/library/base/html/rapply.html
  out <- c(control$iep_list$GROUPVARS,
           control$gifted_list$GROUPVARS,
           control$ses_list$GROUPVARS)
  unique(out)
}


#' Generate a roster of schools to assign students to
#'
#' @param n number of schools
#' @param mean a vector of means for the school attributes
#' @param sigma a covariance matrix for the school attributes
#' @param names vector to draw names from
#'
#' @return a data.frame with schools and their attributes
#' @importFrom mvtnorm rmvnorm
#' @export
gen_schools <- function(n, mean = NULL, sigma = NULL, names = NULL){
  if(missing(mean)){
    mean_vec <- structure(
      c(0, 0, 0, 0, 0),
      .Names = c("male_per",
                 "frpl_per", "sped_per", "lep_per", "gifted_per")
    )
  } else{
    mean_vec <- mean
  }
  if(missing(sigma)){
    cov_mat <- structure(
      c(rep(0, 25)),
      .Dim = c(5L, 5L),
      .Dimnames = list(
        c("male_per", "frpl_per", "sped_per", "lep_per", "gifted_per"),
        c("male_per","frpl_per", "sped_per", "lep_per", "gifted_per")
      )
    )
  } else{
    cov_mat <- sigma
  }
  if(missing(names)){
    names <- c(LETTERS, letters)
  }
  if(length(n) > length(names)){
    stop("Please add more names or select a smaller n to generate unique names")
  }
  ids <- wakefield::id(n)
  enroll <- rnbinom(n, size = 0.5804251, mu = 360.8085106) # starting values from existing district
  names <- sample(names, size = n, replace = FALSE)
  attribs <- mvtnorm::rmvnorm(n, mean = mean_vec, sigma = cov_mat)
  attribs[attribs < 0] <- 0
  attribs[attribs >=1] <- 0.99
  K <- length(enroll[enroll == 0])
  enroll[enroll == 0] <- sample(1:25, K, replace = FALSE)
  out <- data.frame(schid = ids, name = names, enroll = enroll,
                    stringsAsFactors = FALSE)
  out <- cbind(out, attribs)
  return(out)
}


#' Assign student enrollment spells to a school ID
#'
#' @param student data frame of student-year observations
#' @param schools data frame of schools to assign from
#' @param method currently unused, will allow for different assignment methods
#'
#' @return student, with additional column schid appended
#' @export
assign_schools <- function(student, schools, method = NULL){
  # TODO thoroughly test inputs to make sure year exists
  # TODO test that school ids match from schools
  # Assignment techniques -- purely ignorant weighting technique
  # Non-ignorant, weighted technique
  # Model based technique

  school_t_list <- list(
    "ALL" = list(f = make_markov_series,
                 pars = list(tm = school_transitions(nschls = nrow(schools),
                                                     diag_limit = 0.96))),
    "GROUPVARS" = c("ALL")
  )
  idvar <- names(student)[which(names(student) %in% c("ID", "id", "sid"))]

  student <- student %>% group_by_(idvar) %>% arrange(year) %>%
    mutate(schid = markov_cond_list("ALL", n = n()-1, school_t_list,
                                    t0 = sample(schools$schid, 1, prob = schools$enroll),
                                    include.t0 = TRUE))
  return(student)
}

#' Assign high school outcomes
#'
#' @param g12_cohort a dataframe with certain high school attributes
#' @param control control parameters from the \code{sim_control()} function
#'
#' @return an outcome dataframe
assign_hs_outcomes <- function(g12_cohort, control = sim_control()){
  g12_cohort$scale_gpa <- gen_gpa(data = g12_cohort,
                                  control = control)
  # rescale GPA
  g12_cohort$gpa <- rescale_gpa(g12_cohort$scale_gpa)
  zzz <- gen_grad(data = g12_cohort,
                  control = control)
  g12_cohort <- bind_cols(g12_cohort, zzz)
  g12_cohort$hs_status <- "hs_grad"
  g12_cohort$hs_status[g12_cohort$grad == 0] <-
    sapply(g12_cohort$hs_status[g12_cohort$grad == 0],
           function(x) {
             sample(
               c("dropout", "transferout", "still_enroll", "disappear"),
               1,
               replace = FALSE,
               prob = c(0.62, 0.31, 0.04, 0.03)
             )
           })
  # TODO: Consider filtering this so only HS DIPLOMA eligible
    zzz <- gen_ps(g12_cohort, control = control)
    g12_cohort <- bind_cols(g12_cohort, zzz)
  # ps_eligible <- gen_ps(data = g12_cohort[g12_cohort$grad == 1, ],
  #                       control = control)
  outcomes <- g12_cohort[, c("sid", "scale_gpa", "gpa",
                             "grad_prob", "grad", "hs_status",
                             "ps_prob", "ps")]
  return(outcomes)
}

#' Generate postsecondary institutions
#'
#' @param n number of institutions to generate
#' @param names names to use for schools
#'
#' @return a data.frame of names, IDs, and enrollment weights
#' @export
gen_nsc <- function(n, names = NULL){
  ids <- wakefield::id(n)
  enroll <- rnbinom(n, size = 1.4087, mu = 74.62) # starting values from existing district
  if(missing(names)){
    names <- c(LETTERS, letters)
  }
  if(length(n) > length(names)){
    stop("Please add more names or select a smaller n to generate unique names")
  }
  names <- sample(names, size = n, replace = FALSE)
  # attribs <- mvtnorm::rmvnorm(n, mean = mean_vec, sigma = cov_mat)
  # attribs[attribs < 0] <- 0
  # attribs[attribs >=1] <- 0.99
  K <- length(enroll[enroll == 0])
  enroll[enroll == 0] <- sample(1:25, K, replace = FALSE)
  out <- data.frame(opeid = ids, name = names, enroll = enroll,
                    stringsAsFactors = FALSE)
  # out <- cbind(out, attribs)
  return(out)
}
