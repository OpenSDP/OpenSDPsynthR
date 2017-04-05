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
#' @param nschl integer, number of schools to simulate
#' @param seed integer, random seed to use
#' @param control a list, defined by \code{\link{sim_control}}
#' @return a list with simulated data
#' @importFrom lubridate year
#' @import dplyr
#' @export
#' @examples
#' out <- simpop(nstu = 20, nschl = 2, seed = 213)
simpop <- function(nstu, nschl, seed, control = sim_control()){
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
  #stu_year <- left_join(stu_year, stu_first[, c(1, 6)])
  # Needs to become flexible to multipe inputs and outputs
  # Needs to avoid hardcoding Race transformations, CEDS Xwalk should take
  # place outside of this function
  # This should also be controlled somehow by control eventually
  # message("Assigning ", n, " students to initial ELL status...")
  # stu_first$ell <- gen_initial_status(stu_first, baseline = "ell")
  # message("Assigning ", n, " students to initial SES status...")
  # stu_first$ses <- gen_initial_status(stu_first, baseline = "ses")
  message("Assigning ", nstu, " students to initial FRPL, IEP, and ELL status")
  stu_first <- assign_baseline(baseline = "program", stu_first)
  message("Organizing status variables for you...")
  stu_year <- left_join(stu_year, stu_first[, c(idvar, "ell", "iep", "frpl")],
                        by = idvar)
  # demog_master <- left_join(demog_master, stu_first[, c(idvar, "ses")],
  #                           by = idvar)
  rm(stu_first)
  message("Assigning ", nstu, " students longitudinal status trajectories...")
  cond_vars <- get_sim_groupvars(control)
  stu_year <- left_join(stu_year, demog_master[, c(idvar, cond_vars)],
                        by = idvar)
  stu_year <- gen_annual_status(stu_year, control = control)
  # Create longitudinal ell and ses here
  stu_year <- stu_year %>%
    select_(idvar, "year", "age", "frpl", "ell", "iep", "gifted")
  message("Sorting your records")
  stu_year <- stu_year %>% arrange_(idvar, "year")
  message("Cleaning up...")
  stu_year$age <- round(stu_year$age, 0)
  message("Assiging grades...")
  stu_year <- assign_baseline("grade", stu_year)
  message("Creating ", nschl, " schools for you...")
  school <- gen_schools(n = nschl, mean = control$school_means,
                        sigma = control$school_cov_mat,
                        names = control$school_names)
  message("Success! Returning you student and student-year data in a list.")
  return(list(demog_master = demog_master, stu_year = stu_year,
              schools = school))
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
  reqdVars <- c(reqdVars, c("iep", "ell", "frpl"))
  stopifnot(all(reqdVars %in% names(data)))
  idvar <- names(data)[which(names(data) %in% c("ID", "id", "sid"))]
  data <- data %>% group_by_(idvar) %>% arrange(year) %>%
    mutate(iep = markov_cond_list(Sex[1], n = n()-1, control$iep_list,
                                  t0 = iep[1], include.t0 = TRUE),
           gifted = markov_cond_list(Sex[1], n = n(), control$gifted_list),
           ell = markov_cond_list("ALL", n = n() - 1, control$ell_list,
                                  t0 = ell[1], include.t0 = TRUE),
           frpl = markov_cond_list(Race[1], n = n()-1, control$ses_list,
                                  t0 = frpl[1], include.t0 = TRUE))
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
#' @param race_groups vector of labels for race groups
#' @param race_prob vector of numerics, same length as \code{race_groups}
#' @param minyear an integer
#' @param maxyear an integer
#' @param ses_list a probability list
#' @param iep_list a probability list
#' @param ell_list a probability list
#' @param gifted_list a probability list
#' @param n_cohorts number of cohorts to produce
#' @param school_means a named vector of means for school level attributes
#' @param school_cov_mat a covariance matrix for the school level attributes
#' @param school_names a vector to draw school names from
#' @return a named list
#' @export
sim_control <- function(race_groups=NULL, race_prob=NULL,
                        ses_list=NULL, minyear=1997, maxyear=2017,
                        n_cohorts = NULL, gifted_list=NULL, iep_list=NULL,
                        ell_list=NULL, school_means=NULL, school_cov_mat=NULL,
                        school_names=NULL){

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
    c(250, 50, 150, 900),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(c("1", "0"), c("1", "0"))
  )
  tm_iep_m <- tm_iep_f
  tm_iep_m[, 1] <- tm_iep_m[, 1] + 50
  tm_iep_m <- tm_iep_m / rowSums(tm_iep_m)
  tm_iep_f <- tm_iep_f / rowSums(tm_iep_f)

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

    school_names <- sch_names

    structure(namedList(
                 race_groups,
                 race_prob,
                 minyear,
                 maxyear,
                 gifted_list,
                 iep_list,
                 ses_list,
                 ell_list,
                 n_cohorts,
                 school_means,
                 school_cov_mat,
                 school_names))

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

#' Generate data for a multilevel outcome
#'
#' @param fixed a formula with RHS only that specifies the variables to use
#' @param fixed_param a vector of numerics for the coefficients of variables in fixed
#' @param random_var a numeric, length 1, variance of random (school level) component
#' @param cov_param a list, defining any continuous variables
#' @param cor_vars correlations between fixed variables
#' @param fact_vars for each variable in fixed that is a factor, a definition...
#' @param ngrps number of schools
#' @param unbalanceRange range of enrollments in each school
#' @importFrom simglm sim_glm
#'
#' @return a data.frame
#' @export
#'
#' @examples
#' zed2 <- gen_outcome_model(fixed = ~ 1 + gifted.f + iep.f + frpl.f + ell.f + male.f,
#' random_var = 0.77, fixed_param = c(1.06, 0.72, -.2, -0.513, -0.4559, -0.356),
#' fact_vars = list(numlevels = c(2, 2, 2, 2, 2), var_type = c(rep('lvl1', 5))),
#' ngrps = 20, unbalanceRange = c(75, 900))
gen_outcome_model <- function(fixed, fixed_param, random_var, fact_vars, cov_param = NULL,
                              cor_vars = NULL,
                              ngrps, unbalanceRange){
  #fixed <- ~ 1 + gifted.f + iep.f + frpl.f + ell.f + male.f
  random <- ~ 1
  random_param <- list(random_var = random_var, rand_gen = "rnorm")
  # cor_vars <- NULL # needs to be the length of all correlations between predictors
  # fixed_param <- c(1.06, 0.72, -0.20, -0.513, -0.4669, -0.356)
  # fact_vars <- list(numlevels = c(2, 2, 2, 2, 2), var_type = c(rep('lvl1', 5)))
  # random_param <- list(random_var = c(0.7728), rand_gen = 'rnorm') # intercept + any slopes in length
  # unbalCont <- c(100, 600)
  # Total number of level 2 groups = k * n
  # n <- 15 # obs per group level 2 group
  #p <- 400 # obs per group?
  # data_str <- "long"
  # cov_param <- NULL
  df <- sim_glm(fixed = fixed, random = random,
                fixed_param = fixed_param, random_param = random_param,
                random3 = NULL,
                random_param3 = NULL,
                cov_param = cov_param,
                fact_vars = fact_vars, k = NULL,
                n = ngrps, p = NULL,
                cor_vars = cor_vars, data_str = "cross", unbal = TRUE,
                unbalCont = unbalanceRange)
  return(df)

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


