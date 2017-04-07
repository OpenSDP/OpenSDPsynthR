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
  message("Simulating high school outcomes... be patient...")
  g12_cohort <- stu_year[stu_year$grade == "12", ]
  g12_cohort <- na.omit(g12_cohort)
  g12_cohort <- left_join(g12_cohort, demog_master[, 1:4], by = idvar)
  g12_cohort$male <- ifelse(g12_cohort$Sex == "Male", 1, 0)
  hs_outcomes <- assign_hs_outcomes(g12_cohort, control = control)
  message("Success! Returning you student and student-year data in a list.")
  return(list(demog_master = demog_master, stu_year = stu_year,
              schools = school, hs_outcomes = hs_outcomes))
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


#' Set control parameters for simulated data
#' @param nschls integer for number of schools to create, default is 2
#' @param race_groups vector of labels for race groups
#' @param race_prob vector of numerics, same length as \code{race_groups}
#' @param minyear an integer
#' @param maxyear an integer
#' @param ses_list a probability list
#' @param iep_list a probability list
#' @param ell_list a probability list
#' @param grade_levels a probability list
#' @param gifted_list a probability list
#' @param n_cohorts number of cohorts to produce
#' @param school_means a named vector of means for school level attributes
#' @param school_cov_mat a covariance matrix for the school level attributes
#' @param school_names a vector to draw school names from
#' @param gpa_sim_parameters a list of parameters to pass to \code{gen_outcome_model}
#' @param grad_sim_parameters a list of parameters to pass to \code{gen_outcome_model}
#' @return a named list
#' @export
sim_control <- function(nschls=2L, race_groups=NULL, race_prob=NULL,
                        ses_list=NULL, minyear=1997, maxyear=2017,
                        n_cohorts = NULL, gifted_list=NULL, iep_list=NULL,
                        ell_list=NULL, grade_levels=NULL, school_means=NULL, school_cov_mat=NULL,
                        school_names=NULL, gpa_sim_parameters=NULL,
                        grad_sim_parameters=NULL){
  nschls <- nschls

  # temporarily hardcoding these values here for testing
  tm_gifted_f <- matrix(
    c(500, 1, 2, 500),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(c("1", "0"), c("1", "0"))
  )
  tm_gifted_m <- tm_gifted_f
  tm_gifted_m[1, 1] <- tm_gifted_m[1, 1] + 25
  tm_gifted_m <- tm_gifted_m / rowSums(tm_gifted_m)
  tm_gifted_f <- tm_gifted_f / rowSums(tm_gifted_f)

  gifted_list <- list(
    "Male" = list(f = make_markov_series,
                  pars = list(tm = tm_gifted_m,
                              # Use quote so for each call in the loop sample is redrawn
                              t0 = quote(
                                sample(c("1", "0"), 1, prob = c(10, 90))
                              ))),
    "Female" = list(f = make_markov_series,
                    pars = list(tm = tm_gifted_f,
                                t0 = quote(
                                  sample(c("1", "0"), 1, prob = c(8, 92))
                                ))),
    "GROUPVARS" = c("Sex")
  )

  # IEP
  tm_iep_f <- matrix(
    c(650, 50, 15, 900),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(c("1", "0"), c("1", "0"))
  )
  tm_iep_m <- tm_iep_f
  tm_iep_m[, 1] <- tm_iep_m[, 1] + 50
  tm_iep_m <- tm_convert(tm_iep_m)
  tm_iep_f <- tm_convert(tm_iep_f)

  iep_list <- list(
    "Male" = list(f = make_markov_series,
                  pars = list(tm = tm_iep_m)),
    "Female" = list(f = make_markov_series,
                    pars = list(tm = tm_iep_f)),
    "GROUPVARS" = c("Sex")
  )

  tm <- matrix(
    c(800, 20, 5, 800),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(c("1", "0"), c("1", "0"))
  )
  tm <- tm / rowSums(tm)
  ell_list <- list(
    "ALL" = list(f = make_markov_series,
                 pars = list(tm = tm)),
    "GROUPVARS" = c("ell")
  )

  tm <- grade_transitions(ngrades = 15L, diag_limit = 0.95)
  grade_levels <- list(
    "ALL" = list(f = make_markov_series,
                 pars = list(tm = tm)),
    "GROUPVARS" = c("grade")
  )

  ses_dim <- list(c("0", "1"), c("0", "1"))
    race_ses_tm <- structure(
      list(
        White = structure(
          c(
            0.91859191329348,
            0.112994957427461,
            0.0814080867065204,
            0.887005042572539
          ),
          .Dim = c(2L, 2L),
          .Dimnames = ses_dim
        ),
        `African American` = structure(
          c(
            0.810682926054717,
            0.0835755631319266,
            0.189317073945283,
            0.916424436868073
          ),
          .Dim = c(2L, 2L),
          .Dimnames = ses_dim
        ),
        `Asian American` = structure(
          c(
            0.935574229691877,
            0.156600974782793,
            0.0644257703081232,
            0.843399025217207
          ),
          .Dim = c(2L, 2L),
          .Dimnames = ses_dim
        ),
        `American Indian` = structure(
          c(
            0.788359788359788,
            0.0721177038109021,
            0.211640211640212,
            0.927882296189098
          ),
          .Dim = c(2L, 2L),
          .Dimnames = ses_dim
        ),
        Multiple = structure(
          c(
            0.891008962127783,
            0.0926444387884958,
            0.108991037872217,
            0.907355561211504
          ),
          .Dim = c(2L, 2L),
          .Dimnames = ses_dim
        ),
        Other = structure(
          c(
            0.835443037974684,
            0.0836363636363636,
            0.164556962025316,
            0.916363636363636
          ),
          .Dim = c(2L, 2L),
          .Dimnames = ses_dim
        )
      ),
      .Names = c(
        "White",
        "African American",
        "Asian American",
        "American Indian",
        "Multiple",
        "Other"
      )
    )

    ses_list <- list(
      "White" = list(f = make_markov_series,
                     pars = list(tm = race_ses_tm[["White"]])),
      "Hispanic or Latino Ethnicity" = list(f = make_markov_series,
                                            pars = list(tm = race_ses_tm[["White"]])),
      "Black or African American" = list(f = make_markov_series,
                                         pars = list(tm = race_ses_tm[["African American"]])),
      "Asian" = list(f = make_markov_series,
                     pars = list(tm = race_ses_tm[["Asian American"]])),
      "Demographic Race Two or More Races" = list(f = make_markov_series,
                                                  pars = list(tm = race_ses_tm[["Multiple"]])),
      "American Indian or Alaska Native" = list(f = make_markov_series,
                                                pars = list(tm = race_ses_tm[["American Indian"]])),
      "Other" = list(f = make_markov_series,
                     pars = list(tm = race_ses_tm[["Other"]])),
      "Native Hawaiian or Other Pacific Islander" = list(f = make_markov_series,
                                                         pars = list(tm = race_ses_tm[["American Indian"]])),
      "GROUPVARS" = c("Race")
    )

    school_means <-  structure(
      c(0.513956976686301, 0.618015258420426, 0.111746668239179,
        0.0507135275386248,0.145418091756103),
      .Names = c("male_per",
                 "frpl_per", "sped_per", "lep_per", "gifted_per")
    )
    school_cov_mat <- structure(
      c(0.00626039350019743, 0.00269577401236515, 0.00217008472097632,
        -0.000567299979898903,-0.00514050994295009, 0.00269577401236515,
        0.0553769100560957, 0.00958652234495279, 0.00973569135335458,
        -0.0269235256547852, 0.00217008472097632, 0.00958652234495279,
        0.00760512262055834, -0.000782211474167252, -0.00596964683827119,
        -0.000567299979898903, 0.00973569135335458,-0.000782211474167252,
        0.0176676813362968, -0.00355548955881028, -0.00514050994295009,
        -0.0269235256547852, -0.00596964683827119,-0.00355548955881028,
        0.0307478369601188),
      .Dim = c(5L, 5L),
      .Dimnames = list(
        c("male_per", "frpl_per", "sped_per", "lep_per", "gifted_per"),
        c("male_per","frpl_per", "sped_per", "lep_per", "gifted_per")
      )
    )

    gpa_sim_parameters <- list(
      fixed = ~ 1 + math_ss + gifted + iep + frpl + ell + male,
      random_var = 0.02173,
      cov_param = list(dist_fun = c("rnorm", rep("rbinom", 5)),
                       var_type = rep("lvl1", 6),
                       opts = list(
                         list(mean = 0, sd = 1),
                         list(size = 1, prob =0.1),
                         list(size = 1, prob = 0.2),
                         list(size = 1, prob = 0.45),
                         list(size = 1, prob = 0.1),
                         list(size = 1, prob = 0.47))
      ),
      cor_vars = c(0.453, -0.276, -0.309, -0.046, -0.033, -0.135, -0.210, -0.030, -0.029,
                   0.143, -0.003, 0.127, 0.060, 0.007, 0.001),
      fixed_param = c(0.3799, 0.417892, 0.168458, 0.042588580,
                      -0.289399599, -0.007401886, -0.374127),
      ngrps = nschls, unbalanceRange = c(100, 1500), type = "linear"
    )

    grad_sim_parameters <- list(
      fixed = ~ 1 + math_ss + scale_gpa + gifted + iep + frpl + ell + male,
      random_var = 0.09948,
      cov_param = list(
        dist_fun = c("rnorm", "rnorm", rep("rbinom", 5)),
        var_type = rep("lvl1", 7),
        opts = list(
          list(mean = 0, sd = 1),
          list(mean = 0, sd = 1),
          list(size = 1, prob = 0.1),
          list(size = 1, prob = 0.2),
          list(size = 1, prob = 0.45),
          list(size = 1, prob = 0.1),
          list(size = 1, prob = 0.47)
        )
      ),
      cor_vars = c(
        0.5136, 0.453, -0.276, -0.309, -0.046, -0.033,
        0.2890, -0.1404, -0.2674, -0.0352,-0.1992,
        -0.1354, -0.2096, -0.0305, -0.0290,
        0.1433, -0.0031, 0.1269,
        0.0601, 0.0066,
        0.0009
      ),
      fixed_param = c(
        1.7816, 0.10764, 1.05872, -0.07352, -0.07959,
        -0.331647,-0.22318254, 0.0590
        ),
      ngrps = 30,
      unbalanceRange = c(100, 1500)
    )

    school_names <- sch_names

    structure(namedList(
                 nschls,
                 race_groups,
                 race_prob,
                 minyear,
                 maxyear,
                 gifted_list,
                 iep_list,
                 ses_list,
                 ell_list,
                 grade_levels,
                 n_cohorts,
                 school_means,
                 school_cov_mat,
                 school_names,
                 gpa_sim_parameters,
                 grad_sim_parameters))

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
  outcomes <- g12_cohort[, c("sid", "scale_gpa", "gpa",
                             "grad_prob", "grad", "hs_status")]
  return(outcomes)
}

