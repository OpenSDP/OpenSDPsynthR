
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
    df <- sim_glm(fixed = fixed, random = random,
                  fixed_param = fixed_param, random_param = random_param,
                  random3 = NULL,
                  random_param3 = NULL,
                  cov_param = cov_param,
                  fact_vars = fact_vars, k = NULL,
                  n = ngrps, p = NULL,
                  cor_vars = cor_vars, data_str = "cross", unbal = TRUE,
                  unbalCont = unbalanceRange)
  } else if(type == "linear"){
    if(missing(error_var)){
      error_var <- 1.75
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
  }

  return(df)

}
