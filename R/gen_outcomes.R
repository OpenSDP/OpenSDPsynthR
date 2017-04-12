
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
#' @param type character, either "binary" or "linear" to choose outcome variable
#' type to generate
#' @param with_err_gen name of a distribution function to generate the errors, optional
#' @param error_var integer values to pass to err_gen, optional
#' @importFrom simglm sim_glm
#' @importFrom simglm sim_reg
#' @import lme4
#' @return a list with two elements
#' @export
#'
#' @examples
#' zed2 <- do.call(gen_outcome_model, sim_control()$gpa_sim_parameters)
gen_outcome_model <- function(fixed, fixed_param, random_var, fact_vars,
                              cov_param = NULL,
                              cor_vars = NULL,
                              ngrps, unbalanceRange,
                              type = "binary", with_err_gen = NULL, error_var = NULL){
  #fixed <- ~ 1 + gifted.f + iep.f + frpl.f + ell.f + male.f
  random <- ~ 1
  random_param <- list(random_var = random_var, rand_gen = "rnorm")
  # Replace factors with binary values so correlation structure is captured
  # cor_vars should be the upper or lower triangle of the correlation matrix of all
  # fixed predictors
  # Fills left to right, first row first, second row second, etc.
  # fixed_param <- c(1.06, 0.72, -0.20, -0.513, -0.4669, -0.356)
  # fact_vars <- list(numlevels = c(2, 2, 2, 2, 2), var_type = c(rep('lvl1', 5)))
  # random_param <- list(random_var = c(0.7728), rand_gen = 'rnorm') # intercept + any slopes in length
  # unbalCont <- c(100, 600)
  # Total number of level 2 groups = k * n
  # n <- 15 # obs per group level 2 group
  #p <- 400 # obs per group?
  # data_str <- "long"
  # cov_param <- NULL
  if(type == "binary"){
    # TODO - document that an omitted variable is generated and then ignored
    # in the second stage
    df <- sim_glm(fixed = fixed, random = random,
                  fixed_param = fixed_param, random_param = random_param,
                  random3 = NULL,
                  random_param3 = NULL,
                  cov_param = cov_param,
                  fact_vars = fact_vars, k = NULL,
                  n = ngrps, p = NULL,
                  cor_vars = cor_vars, data_str = "cross", unbal = TRUE,
                  unbalCont = unbalanceRange)
    mod <- glmer(update(fixed, "sim_data ~ . - math_ss + (1|clustID)"),
                 data = df, family = "binomial")
  } else if(type == "linear"){
    if(missing(error_var)){
      error_var <- 2.5
    }
    if(missing(with_err_gen)){
      with_err_gen <- "rnorm"
    }
      df <- sim_reg(fixed = fixed, random = random,
                  fixed_param = fixed_param, random_param = random_param,
                  random3 = NULL,
                  random_param3 = NULL,
                  cov_param = cov_param,
                  fact_vars = fact_vars, k = NULL,
                  n = ngrps, p = NULL,
                  cor_vars = cor_vars, data_str = "cross", unbal = TRUE,
                  unbalCont = unbalanceRange,
                  error_var = error_var, with_err_gen = with_err_gen)
      mod <- lmer(update(fixed, "sim_data ~ . - math_ss + (1|clustID)"),
                   data = df)
  }

  return(list(sim_model = mod, sim_data = df))

}


#' Generate a final GPA for students
#'
#' @param data a dataframe with variables
#' @param control a sim_control parmeter, default is \code{sim_control}
#'
#' @return a numeric vector
#' @export
gen_gpa <- function(data, control=sim_control()){
  data <- as.data.frame(data)
  if(control$gpa_sim_parameters$ngrps != control$nschls){
    warning("Changing number of groups in outcome simulation to match schools")
    control$gpa_sim_parameters$ngrps <- control$nschls
  }
  gpa_sim <- do.call(gen_outcome_model, control$gpa_sim_parameters)
  if(any(all.vars(control$gpa_sim_parameters$fixed) %in% names(data))){
    warning("Data may not line up")
  }
  idvar <- names(data)[which(names(data) %in%
                                       c("SCH", "schid"))]
  data$clustID <- as.numeric(data[, idvar])
  # g12_cohort$gpa <- predict(gpa_mod, newdata = g12_cohort)
  zed <- simulate(gpa_sim$sim_model, nsim = 500, newdata = data)
  out <- apply(zed, 1, function(x) sample(x, 1))
  # Export
  return(out)
}

#' Generate a high school graduation for students
#'
#' @param data a dataframe with variables
#' @param control a sim_control parmeter, default is \code{sim_control}
#'
#' @return a data.frame with two values, a probability and a binary outcome
#' @export
gen_grad <- function(data, control = sim_control()){
  data <- as.data.frame(data)
  if(control$grad_sim_parameters$ngrps != control$nschls){
    warning("Changing number of groups in outcome simulation to match schools")
    control$grad_sim_parameters$ngrps <- control$nschls
  }
  grad_sim <- do.call(gen_outcome_model, control$grad_sim_parameters)
  if(any(all.vars(control$grad_sim_parameters$fixed) %in% names(data))){
    warning("Data may not line up")
  }
  idvar <- names(data)[which(names(data) %in%
                               c("SCH", "schid"))]
  data$clustID <- as.numeric(data[, idvar])
  # g12_cohort$gpa <- predict(gpa_mod, newdata = g12_cohort)
  zed <- simulate(grad_sim$sim_model, nsim = 500, newdata = data,
                  family = "binomial")
  out_prob <- apply(zed, 1, mean)
  out_binom <- sapply(out_prob, function(x) rbinom(1, 1, x))
  # Export
  out <- data.frame(grad_prob = out_prob, grad = out_binom)
  return(out)
}

#' Rescale scaled GPA to be on 0-4 scale
#'
#' @param x a scaled normal variable
#'
#' @return a GPA rounded to tenths, from 0 to 1
#' @export
rescale_gpa <- function(x){
  # Rescale to be on GPA scale
  x <- unscale(x, mean = 2.4266, sd = 0.85406)
  x <- ifelse(x < 0, 0.25, x)
  x <- ifelse(x > 4, 4, x)
  x <- round(x, 1)
  return(x)
}

#' Generate postsecondary enrollment outcome
#'
#' @param data cohort data
#' @param control output from \code{sim_control}
#'
#' @return a two-column dataframe with probabilities and binary outcome
#' @export
gen_ps <- function(data, control = sim_control()){
  data <- as.data.frame(data)
  if(control$grad_sim_parameters$ngrps != control$nschls){
    warning("Changing number of groups in outcome simulation to match schools")
    control$grad_sim_parameters$ngrps <- control$nschls
  }
  ps_sim <- do.call(gen_outcome_model, control$ps_sim_parameters)
  if(any(all.vars(control$grad_sim_parameters$fixed) %in% names(data))){
    warning("Data may not line up")
  }
  idvar <- names(data)[which(names(data) %in%
                               c("SCH", "schid"))]
  data$clustID <- as.numeric(data[, idvar])
  # g12_cohort$gpa <- predict(gpa_mod, newdata = g12_cohort)
  zed <- simulate(ps_sim$sim_model, nsim = 500, newdata = data,
                  family = "binomial")
  out_prob <- apply(zed, 1, mean)
  out_binom <- sapply(out_prob, function(x) rbinom(1, 1, x))
  # Export
  out <- data.frame(ps_prob = out_prob, ps = out_binom)
  return(out)
}


#' Generate an assessment table
#'
#' @param data student-year data
#' @param control output from \code{sim_control}
#'
#' @return a two-column dataframe with math and reading scores
#' @export
gen_assess <- function(data, control = sim_control()){
  data <- as.data.frame(data)
  df <- do.call(sim_reg, control$assess_sim_par, quote = TRUE)
  mod <- lmer(update(control$assess_sim_par$fixed, "sim_data ~ . + (1|clustID) + (1|clust3ID)"),
              data = df)
  mod <- lmer(update(control$assess_sim_par$fixed, "sim_data ~ . + (1+time|clustID) +
                     (1+time|clust3ID)"),
              data = df)

  sch_id_var <- names(data)[which(names(data) %in%
                               c("SCH", "schid"))]
  stu_id_var <- names(data)[which(names(data) %in%
                                    c("sid", "stuid", "stu"))]
  data$clust3ID <- as.numeric(data[, sch_id_var])
  data$clustID <- as.numeric(data[, stu_id_var])
  #TODO: Decide what to do, peg time to age or to grade
  data$time <- data$age - min(data$age)
  # Need to normalize time
  zed <- simulate(mod, nsim = 500, newdata = data)
  # math <- apply(zed, 1, function(x) (sample(x, 1) + mean(x)) / 2)
  math <- apply(zed, 1, function(x) sample(sort(x)[100:400], 1))
  zed <- simulate(mod, nsim = 500, newdata = data)
  # rdg <- apply(zed, 1, function(x) (sample(x, 1) + mean(x)) / 2)
  rdg <- apply(zed, 1, function(x) sample(sort(x)[100:400], 1))
  out <- data.frame(math_ss = math, rdg_ss = rdg)
  return(out)
}


