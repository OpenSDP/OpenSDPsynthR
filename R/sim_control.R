#' Set control parameters for simulated data
#' @param nschls integer- number of schools to create, default is 2
#' @param best_school character, format is a number, padded with a leading 0,
#' indicates the school that will be the highest performing
#' @param race_groups character - vector of labels for race groups
#' @param race_prob numerics - same length as \code{race_groups}
#' @param minyear integer - the first year student records are observed
#' @param maxyear integer - the last year student records are observed
#' @param ses_list a probability list defining probabilities of being low
#' socioeconomic status, see Details
#' @param iep_list a probability list defining probabilities of being on an
#' individualized education plan
#' @param ell_list a probability list defining probabilities for being an English
#' language learner
#' @param ps_transfer_list a probability list for transferring postsecondary
#' institutions after enrolling
#' @param grade_levels a probability list for grade promotion and retention
#' @param gifted_list a probability list defining the probability of being in
#' a gifted and talented program
#' @param n_cohorts integer - the number of birth-year cohorts to produce
#' @param school_means numeric - a named vector of means for school level attributes
#' @param school_cov_mat matrix - a covariance matrix for the school level attributes
#' @param school_names character - a vector to draw school names from
#' @param postsec_names character - a vector to draw postsecondary institution
#' names from
#' @param gpa_sim_parameters list - parameters to pass to \code{gen_outcome_model}
#' @param grad_sim_parameters list - parameters to pass to \code{gen_outcome_model}
#' @param ps_sim_parameters list - parameters to pass to \code{gen_outcome_model}
#' @param assess_sim_par list - parameters to pass to \code{gen_outcome_model}
#' @param assessment_adjustment list - parameters to adjust assessment scores by
#' for bias
#' @param grad_adjustment list - parameters to adjust graduation probabilities by
#' for bias
#' @param ps_adjustment list - parameters to adjust postsecondary enrollment
#' probabilities by for bias
#' @param gpa_adjustment list - parameters to adjust gpa for bias
#' @param assess_grades character - grade levels to generate assessment scores for
#' @param n_postsec numeric - number of postsecondary schools to assign to
#' @param postsec_method character - options "scorecard" or NULL
#' @details This function has a full set of default values that are designed to
#' produce realistic data. These defaults can be overridden by specifying any
#' of the arguments to be overridden as an option to the function call.
#'
#' There are two unique data structures that are used to set options for simulations.
#' The first is a \code{probability_list}, a list which defines a grouping factor, and
#' for each level of the grouping factor a function and parameters to generate
#' probability distribution from.
#'
#' The \code{sim_parameters} data structure defines the parameters for the outcome
#' simulation. Outcomes are simulated using a simulated multilevel model structure
#' and this data structure contains the parameters that describe the model and the
#' error structure of data generated from that model.
#'
#' To modify either of these elements, use the \code{\link{validate_probability_list}}
#' or \code{\link{validate_sim_parameter}} helper functions to ensure that all
#' of the parameters are defined with valid values.
#' @return a named list
#' @export
sim_control <- function(nschls=2L, best_school= NULL, race_groups=NULL, race_prob=NULL,
                        ses_list=NULL, minyear=2002, maxyear=2017,
                        n_cohorts = 8L, gifted_list=NULL, iep_list=NULL,
                        ell_list=NULL, ps_transfer_list=NULL,
                        grade_levels=NULL, school_means=NULL, school_cov_mat=NULL,
                        school_names=NULL, postsec_names=NULL, gpa_sim_parameters=NULL,
                        grad_sim_parameters=NULL,
                        ps_sim_parameters = NULL,
                        assess_sim_par = NULL,
                        assessment_adjustment = NULL,
                        grad_adjustment = NULL,
                        ps_adjustment = NULL,
                        gpa_adjustment = NULL,
                        assess_grades = NULL,
                        n_postsec=35L,
                        postsec_method = "scorecard"){

  nschls <- nschls
  if(is.null(best_school)){
    best_schl <- "01"
  } else {
    best_schl <- best_school
  }

  if(is.null(race_groups)){
    race_groups <- xwalk$CEDS_name[xwalk$category == "Demographic"]
    race_groups <- race_groups[!race_groups %in% c("Sex", "","Birthdate")]
    race_groups <- as.character(race_groups)
  }
  if(is.null(race_prob)){
    race_prob <- c(0.637, 0.047, 0.007, 0.122, 0.163, 0.021, 0.0015)
  }
  assert(length(race_groups) == length(race_prob),
            "Race groups and race probabilities must be identical length")
  if(is.null(gifted_list)){
    # temporarily hardcoding these values here for testing
    tm_gifted_f <- matrix(
      c(500, 1, 2, 500),
      nrow = 2,
      byrow = TRUE,
      dimnames = list(c("1", "0"), c("1", "0"))
    )
    tm_gifted_m <- tm_gifted_f
    tm_gifted_m[1, 1] <- tm_gifted_m[1, 1] + 25
    tm_gifted_m <- tm_convert(tm_gifted_m)
    tm_gifted_f <- tm_convert(tm_gifted_f)
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
  } else{
    assert(validate_probability_list(gifted_list),
              "Invalid simulation parameters for gifted student status probability.")
  }
  # IEP
  if(is.null(iep_list)){
    tm_iep_f <- matrix(
      c(750, 7, 7, 1100),
      nrow = 2,
      byrow = TRUE,
      dimnames = list(c("1", "0"), c("1", "0"))
    )
    tm_iep_m <- tm_iep_f
    tm_iep_m[, 1] <- tm_iep_m[, 1] + 25
    tm_iep_m <- tm_convert(tm_iep_m)
    tm_iep_f <- tm_convert(tm_iep_f)

    iep_list <- list(
      "Male" = list(f = make_markov_series,
                    pars = list(tm = tm_iep_m)),
      "Female" = list(f = make_markov_series,
                      pars = list(tm = tm_iep_f)),
      "GROUPVARS" = c("Sex")
    )
  } else{
    assert(validate_probability_list(iep_list),
              "Invalid simulation parameters for IEP student status probability.")
  }
  # ELL list
  if(is.null(ell_list)){
    tm <- matrix(
      c(800, 20, 5, 800),
      nrow = 2,
      byrow = TRUE,
      dimnames = list(c("1", "0"), c("1", "0"))
    )
    tm <- tm_convert(tm)
    ell_list <- list(
      "ALL" = list(f = make_markov_series,
                   pars = list(tm = tm)),
      "GROUPVARS" = c("ell")
    )
  } else{
    assert(validate_probability_list(ell_list),
              "Invalid simulation parameters for ELL student status probability.")
  }
  # Grade Levels
  if(is.null(grade_levels)){
    tm <- grade_transitions(ngrades = 15L, diag_limit = 0.9875)
    grade_levels <- list(
      "ALL" = list(f = make_markov_series,
                   pars = list(tm = tm)),
      "GROUPVARS" = c("grade")
    )
  } else{
    assert(validate_probability_list(grade_levels),
              "Invalid simulation parameters for grade level status probability.")
  }
  # SES
  if(is.null(ses_list)){

    ses_dim <- list(c("0", "1"), c("0", "1"))
    race_ses_tm <- structure(
      list(
        White = structure(
          c(
            0.94859191329348,
            0.142994957427461,
            0.0614080867065204,
            0.847005042572539
          ),
          .Dim = c(2L, 2L),
          .Dimnames = ses_dim
        ),
        `African American` = structure(
          c(
            0.840682926054717,
            0.0635755631319266,
            0.149317073945283,
            0.926424436868073
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
            0.935443037974684,
            0.1536363636363636,
            0.064556962025316,
            0.846363636363636
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
                     pars = list(tm = tm_convert(race_ses_tm[["White"]]))),
      "Hispanic or Latino Ethnicity" = list(f = make_markov_series,
                                            pars = list(tm = tm_convert(race_ses_tm[["Other"]]))),
      "Black or African American" = list(f = make_markov_series,
                                         pars = list(tm = tm_convert(race_ses_tm[["African American"]]))),
      "Asian" = list(f = make_markov_series,
                     pars = list(tm = race_ses_tm[["Asian American"]])),
      "Demographic Race Two or More Races" = list(f = make_markov_series,
                                                  pars = list(tm = race_ses_tm[["Multiple"]])),
      "American Indian or Alaska Native" = list(f = make_markov_series,
                                                pars = list(tm = tm_convert(race_ses_tm[["American Indian"]]))),
      "Other" = list(f = make_markov_series,
                     pars = list(tm = race_ses_tm[["Other"]])),
      "Native Hawaiian or Other Pacific Islander" = list(f = make_markov_series,
                                                         pars = list(tm = tm_convert(race_ses_tm[["American Indian"]]))),
      "GROUPVARS" = c("Race")
    )
  } else{
    assert(validate_probability_list(ses_list),
              "Invalid simulation parameters for socioeconomic status probability.")
  }
  #
  if(is.null(school_means)){
    school_means <-  structure(
      c(0.513956976686301, 0.618015258420426, 0.111746668239179,
        0.0507135275386248,0.145418091756103),
      .Names = c("male_per",
                 "frpl_per", "sped_per", "lep_per", "gifted_per")
    )
  }
  if(is.null(school_cov_mat)){
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
  }
  if(is.null(gpa_sim_parameters)){
    gpa_sim_parameters <- list(
      fixed = ~ 1 + math_ss + gifted + iep + frpl + ell + male,
      random_var = 0.2173,
      cov_param = list(dist_fun = c("rnorm", rep("rbinom", 5)),
                       var_type = rep("lvl1", 6),
                       opts = list(
                         list(mean = 0, sd = 1),
                         list(size = 1, prob = 0.1),
                         list(size = 1, prob = 0.2),
                         list(size = 1, prob = 0.45),
                         list(size = 1, prob = 0.1),
                         list(size = 1, prob = 0.47))
      ),
      cor_vars = c(0.453, -0.276, -0.309, -0.046, -0.033, -0.135, -0.210, -0.030, -0.029,
                   0.143, -0.003, 0.127, 0.060, 0.007, 0.001),
      fixed_param = c(0.0799, 0.577892, 0.108458, 0.042588580,
                      -0.289399599, -0.007401886, -0.374127),
      ngrps = nschls + 5, unbalanceRange = c(100, 1500), type = "linear"
    )
  } else{
    assert(validate_sim_parameter(gpa_sim_parameters),
              "Invalid simulation parameters for GPA")
  }
  if(is.null(grad_sim_parameters)){
    grad_sim_parameters <- list(
      fixed = ~ 1 + math_ss + scale_gpa + gifted + iep + frpl + ell + male,
      random_var = 0.8948,
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
        1.6816, 0.30764, 1.05872, -0.07352, -0.07959,
        -0.331647,-0.22318254, 0.0590
      ),
      ngrps = nschls + 5,
      unbalanceRange = c(100, 1500)
    )
  } else{
    assert(validate_sim_parameter(grad_sim_parameters),
              "Invalid simulation parameters for graduation")
  }
  if(is.null(ps_sim_parameters)){
    ps_sim_parameters <- list(
      fixed = ~ 1 +  math_ss + scale_gpa + gifted + iep + frpl + ell + male,
      random_var = 0.1713,
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
        -0.3921, 0.50401, 1.125067, 0.11834, -0.561429,
        -0.31560,-0.126704, -0.03387
      ),
      ngrps = nschls + 5,
      unbalanceRange = c(100, 1500)
    )
  } else{
    assert(validate_sim_parameter(ps_sim_parameters),
              "Invalid simulation parameters for postsecondary enrollment")
  }
  if(is.null(assess_sim_par)){
    assess_sim_par <- list(
      fixed = ~ 1 + time + gifted + iep + frpl + ell + male,
      random = ~ 1 + time,
      random3 = ~ 1 + time,
      cor_vars = c(-0.276, -0.309, -0.046, -0.033,
                   -0.03, -0.029, -0.003, 0.06, 0.007, 0.001),
      fixed_param = c(-0.250245, 0.75, 0.10, -0.161388, -0.075, -0.056, 0.007),
      fact_vars = NULL,
      # intercept + any slopes in length
      # Between student
      random_param = list(random_var = c(0.4, 0.125), cor_vars = c(-0.4), rand_gen = 'rnorm'),
      # Between school
      random_param3 = list(random_var = c(1.2, 0.4), cor_vars = c(-0.4), rand_gen = 'rnorm'), # intercept + any slopes in length
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
      k = nschls + 5, # level 3 groups
      n = 200, # obs per group level 2 group
      p = 400, # obs per group?
      error_var = 1,
      with_err_gen = 'rnorm',
      lvl1_err_params = list(mean = 0, sd = 1),
      data_str = "long"
    )
  } else{
    assert(validate_sim_parameter(assess_sim_par),
              "Invalid simulation parameters for assessment scores")
  }
  if (is.null(ps_transfer_list)) {
    tm <- matrix(
      c(100, 700, 90, 900),
      nrow = 2,
      byrow = TRUE,
      dimnames = list(c("1", "0"), c("1", "0"))
    )
    ps_transfer_list <- list("ALL" =
                               list(f = make_markov_series,
                                    pars = list(tm = tm_convert(tm))))
  } else{
    assert(validate_probability_list(ps_transfer_list),
              "Invalid simulation parameters for postsecondary transfer probability.")
  }
  if (is.null(assessment_adjustment)) {
    assessment_adjustment <- list(
      race_list = list(
        "White" = 0.2,
        "Black or African American" = -0.4,
        "Asian" = 0.2,
        "Hispanic or Latino Ethnicity" = -0.25,
        "Demographic Race Two or More Races" = -0.1,
        "American Indian or Alaska Native" = -0.25,
        "Native Hawaiian or Other Pacific Islander" = -0.05
      ),
      perturb_race = function(x, race, sd, race_par = race_list){
        dist_mean <- race_par[[which(race == names(race_par))]] * sd
        dist_sd <- abs(sd * (dist_mean/sd))
        y <- x + rnorm(1, dist_mean, sqrt(dist_sd^3))
        return(y)
      },
      gender_list = list("Male" = 0, # default = no gender bias
                          "Female" = 0),
      perturb_gender = function(x, gender, sd, gender_par = gender_list) {
        dist_mean <- gender_par[[which(gender == names(gender_par))]] * sd
        dist_sd <- abs(sd * (dist_mean/sd))
        y <- x + rnorm(1, dist_mean, sqrt(dist_sd))
        return(y)
      },

      frl_list = list("0" = 0.1, "1" = -0.5),
      perturb_frl = function(x, frl, sd, frl_par = frl_list){
        dist_mean <- frl_par[[which(frl == names(frl_par))]] * sd
        dist_sd <- abs(sd * (dist_mean/sd))
        y <- x + rnorm(1, dist_mean, sqrt(dist_sd))
        return(y)
      },
      perturb_base = function(x, sd){
        y <- x + rnorm(1, 0, sd * 0.8)
        return(y)
      },
      school_list = best_schl,
      perturb_school = function(x, schid, sd, schl_par = school_list){
        val_mean <- ifelse(schid %in% schl_par, 0.25, 0)
        val_mean_sd <- abs(sd * (val_mean/sd))
        y <- x + rnorm(1, val_mean, sqrt(val_mean_sd))
        return(y)
      }
    )
  }
  if(is.null(grad_adjustment)){
    grad_adjustment <- list(
      race_list = list(
        "White" = 0.25,
        "Black or African American" = -0.2,
        "Asian" = 0.15,
        "Hispanic or Latino Ethnicity" = -0.2,
        "Demographic Race Two or More Races" = -0.05,
        "American Indian or Alaska Native" = -0.25,
        "Native Hawaiian or Other Pacific Islander" = -0.05
      ),
      perturb_race = function(x, race,race_par = race_list){
        val_mean <- race_par[[which(race == names(race_par))]]
        y <- x + num_clip(rnorm(1, val_mean, sd = 0.075), -0.5, 0.3)
        y <- num_clip(y, 0, 1)
        y <- ifelse(y <= 0, 0.05, y) #failsafes to prevent negative probabilities
        y <- ifelse(y >= 1, 0.985, y) #failsafes to prevent negative probabilities
        return(y)
      },
      frl_list = list("0" = 0.25, "1" = -0.5),
      perturb_frl = function(x, frl, sd, frl_par = frl_list){
        val_mean <- frl_par[[which(frl == names(frl_par))]]
        y <- x + num_clip(rnorm(1, val_mean, sd = 0.075), -0.5, 0.3)
        y <- ifelse(y <= 0, 0.0125, y) #failsafes to prevent negative probabilities
        y <- ifelse(y >= 1, 0.95, y) #failsafes to prevent negative probabilities
        y <- num_clip(y, 0, 1)
        return(y)
      },
      school_list = best_schl,
      perturb_school = function(x, schid, schl_par = school_list){
        val_mean <- ifelse(schid %in% schl_par, 0.3, 0)
        y <- x + rnorm(1, val_mean, sd = 0.05)
        y <- num_clip(y, 0, 1)
        return(y)
      }
    )
  }
  if(is.null(ps_adjustment)){
    ps_adjustment <-  list(
      race_list = list(
        "White" = 0.35,
        "Black or African American" = 0.1,
        "Asian" = 0.2,
        "Hispanic or Latino Ethnicity" = 0.1,
        "Demographic Race Two or More Races" = 0.1,
        "American Indian or Alaska Native" = 0.1,
        "Native Hawaiian or Other Pacific Islander" = 0.1
      ),
      perturb_race = function(x, race,race_par = race_list){
        val_mean <- race_par[[which(race == names(race_par))]]
        y <- x + num_clip(rnorm(1, val_mean, sd = 0.025), -0.1, 0.25)
        y <- num_clip(y, 0, 1)
        y <- ifelse(y <= 0, 0.025, y) #failsafes to prevent negative probabilities
        y <- ifelse(y >= 1, 0.985, y) #failsafes to prevent negative probabilities
        return(y)
      },
      frl_list = list("0" = 0.05, "1" = -0.65),
      perturb_frl = function(x, frl, sd, frl_par = frl_list){
        val_mean <- frl_par[[which(frl == names(frl_par))]]
        y <- x + num_clip(rnorm(1, val_mean, sd = 0.025), -0.1, 0.25)
        y <- num_clip(y, 0, 1)
        y <- ifelse(y <= 0, 0.05, y) #failsafes to prevent negative probabilities
        y <- ifelse(y >= 1, 0.95, y) #failsafes to prevent negative probabilities
        return(y)
      },
      school_list = best_schl,
      perturb_school = function(x, schid, schl_par = school_list){
        val_mean <- ifelse(schid %in% schl_par, 0.25, 0)
        y <- x + rnorm(1, val_mean, sd = 0.05)
        y <- num_clip(y, 0, 1)
        return(y)
      }
    )
  }
  if(is.null(gpa_adjustment)){
    gpa_adjustment <-  list(
      race_list = list(
        "White" = 0.1,
        "Black or African American" = -0.2,
        "Asian" = 0.075,
        "Hispanic or Latino Ethnicity" = -0.2,
        "Demographic Race Two or More Races" = -0.05,
        "American Indian or Alaska Native" = -0.0,
        "Native Hawaiian or Other Pacific Islander" = -0.0
      ),
      perturb_race = function(x, race,race_par = race_list){
        val_mean <- race_par[[which(race == names(race_par))]]
        y <- x + rnorm(1, val_mean, sd = 0.05)
        return(y)
      },
      frl_list = list("0" = 0.07, "1" = -0.175),
      perturb_frl = function(x, frl, sd, frl_par = frl_list){
        val_mean <- frl_par[[which(frl == names(frl_par))]]
        y <- x + rnorm(1, val_mean, sd = 0.075)
        return(y)
      }
    )
  }
  if(is.null(assess_grades)){
    assess_grades <- c("3", "4", "5", "6", "7", "8", "9", "11")
  }
  if(is.null(school_names)){
    school_names <- sch_names
  }
  if(is.null(postsec_names)){
    postsec_names <- ps_names
  }
  structure(namedList(
    nschls,
    best_schl,
    race_groups,
    race_prob,
    minyear,
    maxyear,
    gifted_list,
    iep_list,
    ses_list,
    ell_list,
    ps_transfer_list,
    grade_levels,
    n_cohorts,
    school_means,
    school_cov_mat,
    school_names,
    postsec_names,
    gpa_sim_parameters,
    grad_sim_parameters,
    ps_sim_parameters,
    assess_sim_par,
    assessment_adjustment,
    grad_adjustment,
    ps_adjustment,
    gpa_adjustment,
    assess_grades,
    n_postsec,
    postsec_method))

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
