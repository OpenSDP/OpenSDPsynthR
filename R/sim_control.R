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
#' @param ps_sim_parameters a list of parameters to pass to \code{gen_outcome_model}
#' @param assess_sim_par a list of parameters to pass to \code{gen_outcome_model}
#' @return a named list
#' @export
sim_control <- function(nschls=2L, race_groups=NULL, race_prob=NULL,
                        ses_list=NULL, minyear=2002, maxyear=2017,
                        n_cohorts = NULL, gifted_list=NULL, iep_list=NULL,
                        ell_list=NULL, grade_levels=NULL, school_means=NULL, school_cov_mat=NULL,
                        school_names=NULL, gpa_sim_parameters=NULL,
                        grad_sim_parameters=NULL,
                        ps_sim_parameters = NULL,
                        assess_sim_par = NULL){
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

  ps_sim_parameters <- list(
    fixed = ~ 1 +  math_ss + scale_gpa + gifted + iep + frpl + ell + male,
    random_var = 0.03713,
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
      0.5054, 0.453, -0.2677, -0.3001, -0.0423, -0.0160,
      0.2952, -0.1556, -0.2648, -0.0262,-0.1992,
      -0.1354, -0.2127, -0.0269, -0.0203,
      0.1433, -0.0012, 0.1234,
      0.0601, 0.0016,
      -0.0029
    ),
    fixed_param = c(
      0.0921, 0.20401, 1.125067, 0.11834, -0.561429,
      -0.31560,-0.126704, -0.03387
    ),
    ngrps = 30,
    unbalanceRange = c(100, 1500)
  )
  assess_sim_par <- list(
    fixed = ~ 1 + time + gifted + iep + frpl + ell + male,
    random = ~ 1 + time,
    random3 = ~ 1 + time,
    cor_vars = c(-0.276, -0.309, -0.046, -0.033,
                 -0.03, -0.029, -0.003, 0.06, 0.007, 0.001),
    fixed_param = c(-0.250245, 0.75, 0.10, -0.161388, -0.075, -0.056, 0.007),
    fact_vars = NULL,
    # intercept + any slopes in length
    random_param = list(random_var = c(0.2, 0.1), cor_vars = c(-0.4), rand_gen = 'rnorm'),
    random_param3 = list(random_var = c(0.3, 0.025), cor_vars = c(-0.4), rand_gen = 'rnorm'), # intercept + any slopes in length
    cov_param = list(
      dist_fun = c("rbinom", "rbinom", "rbinom", "rbinom", "rbinom"),
      var_type = rep("lvl1", 5),
      opts = list(
        list(size = 1, prob = 0.1),
        list(size = 1, prob = 0.2),
        list(size = 1, prob = 0.45),
        list(size = 1, prob = 0.1),
        list(size = 1, prob = 0.52)
      )
    ),
    unbalCont = c(2, 16),
    unbalCont3 = c(100, 800),
    unbal = TRUE,
    # Total number of level 2 groups = k * n
    k = 15, # level 3 groups
    n = 200, # obs per group level 2 group
    p = 400, # obs per group?
    error_var = 1,
    with_err_gen = 'rnorm',
    lvl1_err_params = list(mean = 0, sd = 1),
    data_str = "long"
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
    grad_sim_parameters,
    ps_sim_parameters,
    assess_sim_par))

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
