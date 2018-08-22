
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
    df <- sim_glm(fixed = fixed, random = random,
                  fixed_param = fixed_param, random_param = random_param,
                  random3 = NULL,
                  random_param3 = NULL,
                  cov_param = cov_param,
                  fact_vars = fact_vars, k = NULL,
                  n = ngrps, p = NULL,
                  cor_vars = cor_vars, data_str = "cross", unbal = TRUE,
                  unbalCont = unbalanceRange)
    mod <- glmer(update(fixed, "sim_data ~ . + (1|clustID)"),
                 data = df, family = "binomial", nAGQ = 0, # boost speed
                 control=glmerControl(optimizer = "nloptwrap"))
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
      mod <- lmer(update(fixed, "sim_data ~ . + (1|clustID)"),
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
  data$out <- apply(zed, 1, mean)
  # Perturb the gpa
  # FRPL bias is underestimated because of the time component so need to add it in
  # Racial bias is entirely absent gpa_adjustment
  # TODO: test to ensure these values are vectorized and redrawn per observation
  data$out <- mapply(control$gpa_adjustment$perturb_frl,
                          data$out, data$frpl,
                          MoreArgs = list(frl_par = control$gpa_adjustment$frl_list))
  data$out <- mapply(control$gpa_adjustment$perturb_race,
                          data$out, data$Race,
                          MoreArgs = list(race_par = control$gpa_adjustment$race_list))
  return(data$out)
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
  data$out_prob <- apply(zed, 1, mean)
  # TODO: test to ensure these values are vectorized and redrawn per observation
  data$out_prob <- mapply(control$grad_adjustment$perturb_frl,
                          data$out_prob, data$frpl,
                          MoreArgs = list(frl_par = control$grad_adjustment$frl_list))
  data$out_prob <- mapply(control$grad_adjustment$perturb_race,
                         data$out_prob, data$Race,
                         MoreArgs = list(race_par = control$grad_adjustment$race_list))
  data$out_prob <- mapply(control$grad_adjustment$perturb_school,
                          data$out_prob, data[, idvar],
                          MoreArgs = list(schl_par = control$grad_adjustment$school_list))
  data$out_binom <- sapply(data$out_prob, function(x) rbinom(1, 1, x))
  # Export
  out <- data.frame(grad_prob = data$out_prob, grad = data$out_binom)
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
  x <- num_clip(x, min = 0.25, max = 4)
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
  data$out_prob <- apply(zed, 1, mean)
  # TODO: test to ensure these values are vectorized and redrawn per observation
  data$out_prob <- mapply(control$ps_adjustment$perturb_frl,
                          data$out_prob, data$frpl,
                          MoreArgs = list(frl_par = control$ps_adjustment$frl_list))
  data$out_prob <- mapply(control$ps_adjustment$perturb_race,
                          data$out_prob, data$Race,
                          MoreArgs = list(race_par = control$ps_adjustment$race_list))
  data$out_binom <- sapply(data$out_prob, function(x) rbinom(1, 1, x))
  out <- data.frame(ps_prob = data$out_prob, ps = data$out_binom)
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
  # Need to normalize time
  data$time <- data$age - min(data$age)
  zed <- simulate(mod, nsim = 500, newdata = data, allow.new.levels = TRUE)
  # sort each row, then sample from sorted rows, since sort is expensive
  zed <- t(apply(zed, 1, sort))
  # math <- apply(zed, 1, function(x) (sample(x, 1) + mean(x)) / 2)
  math <- apply(zed, 1, function(x) sample(x[225:275], 1))
  rdg <- apply(zed, 1, function(x) sample(x[200:300], 1))
  # No need to run simulation code twice, it is expensive
  # zed <- simulate(mod, nsim = 500, newdata = data, allow.new.levels = TRUE)
  # rdg <- apply(zed, 1, function(x) (sample(x, 1) + mean(x)) / 2)
  data$math_ss <- math
  data$rdg_ss <- rdg
  # Bias parameters need to adjust with the scale of the assessment
  data <- data %>% dplyr::group_by(time) %>%
    dplyr::mutate(math_sd = sd(math_ss),
           rdg_sd = sd(rdg_ss)) %>% as.data.frame()
  # Perturb the test scores to reduce correlation and induce bias
  # FRPL bias is underestimated because of the time component so need to add it in
  # Racial bias is entirely absent
  # TODO add bias by school
  data$math_ss <- mapply(control$assessment_adjustment$perturb_frl,
                         data$math_ss, data$frpl, data$math_sd,
                         MoreArgs = list(frl_par = control$assessment_adjustment$frl_list))
  data$rdg_ss <- mapply(control$assessment_adjustment$perturb_frl,
                        data$rdg_ss, data$frpl, data$rdg_sd,
                        MoreArgs = list(frl_par = control$assessment_adjustment$frl_list))
  data$math_ss <- mapply(control$assessment_adjustment$perturb_race,
                         data$math_ss, data$Race, data$math_sd,
                         MoreArgs = list(race_par = control$assessment_adjustment$race_list))
  data$rdg_ss <- mapply(control$assessment_adjustment$perturb_race,
                        data$rdg_ss, data$Race, data$rdg_sd,
                        MoreArgs = list(race_par = control$assessment_adjustment$race_list))
  data$math_ss <- mapply(control$assessment_adjustment$perturb_school,
                         data$math_ss, data$schid, data$math_sd,
                         MoreArgs = list(schl_par = control$assessment_adjustment$school_list))
  data$rdg_ss <- mapply(control$assessment_adjustment$perturb_school,
                        data$rdg_ss, data$schid, data$rdg_sd,
                        MoreArgs = list(schl_par = control$assessment_adjustment$school_list))
  # Perturb to reduce test correlation between reading and math
  data$rdg_ss <- mapply(control$assessment_adjustment$perturb_base,
                        data$rdg_ss, data$rdg_sd)
  out <- data.frame(math_ss = data$math_ss, rdg_ss = data$rdg_ss)
  return(out)
}


# TODO generalize these two functions to some generic fuzzmatch/generate rmvnorm data
# Sanitize GPA and credits by rounding to tenths and setting ceiling and floor

#' Generate credit sequence for students based on their final GPA
#'
#' @param gpa_ontrack a data.frame containing final GPA for a student
#'
#' @return \code{gpa_ontrack} but appended with credits by year
#' @export
gen_credits <- function(gpa_ontrack){
  cov_matrix <- structure(
    c(
      2.75566945787353, 5.10706978525027, 7.61205992792623, 9.89030594066736,
      0.109029620175826, 0.271951381564673, 0.447968849522654,0.622051838798187,
      0.8021634008576,  0.96205753999336,1.16768443306172,1.233791237022,
      5.10706978525027,  12.1674704877153,19.396615672585, 25.7102906703755,
      0.273879350479468, 0.570042916210459, 1.22749859106289, 1.58296312520297,
      2.21963790263873, 2.50969237429761, 3.22379291455248, 3.19369557755706,
      7.61205992792623, 19.396615672585,  34.0791667678909, 46.2874453943062,
      0.428404096252971,  0.919578332592651, 2.02696519413977, 2.640840313183,
      3.95462602065478, 4.48234313004498, 5.88219242499701, 5.76954297914403,
      9.89030594066736, 25.7102906703755, 46.2874453943062, 66.435397990789,
      0.340522852495716, 1.04313270168342, 2.5177795760211, 3.40109621651257,
      5.2534423057967, 6.06180919017583,  8.23891225133386, 8.26936423977293,
      0.109029620175826, 0.273879350479468, 0.428404096252971, 0.340522852495716,
      0.252057941748993, 0.173152528812616, 0.2926411422342,  0.206823638805838,
      0.324115100929672, 0.220317121847772, 0.361325212075315, 0.162383915833626,
      0.271951381564673,0.570042916210459,       0.919578332592651,   1.04313270168342,
      0.173152528812616, 0.328960279108121, 0.229113790075386, 0.396844816842713,
      0.284275091646359, 0.433223348801798, 0.346406486960778,  0.393770727414352,
      0.447968849522654, 1.22749859106289,  2.02696519413977, 2.5177795760211,
      0.2926411422342,  0.229113790075386, 0.505972511118613, 0.364229637411876,
      0.655560130427122, 0.4697059977753, 0.804111981127887,  0.477170319012822,
      0.622051838798187, 1.58296312520297, 2.640840313183, 3.40109621651257,
      0.206823638805838,  0.396844816842713, 0.364229637411876, 0.649251057339849,
      0.528943932261278,  0.783566748644363,  0.70164285854404, 0.805150232348093,
      0.8021634008576,  2.21963790263873, 3.95462602065478, 5.2534423057967,
      0.324115100929672, 0.284275091646359, 0.655560130427122, 0.528943932261278,
      1.0205017365896, 0.75566986364323, 1.31532906125032,  0.855469148134248,
      0.96205753999336, 2.50969237429761, 4.48234313004498, 6.06180919017583,
      0.220317121847772,  0.433223348801798, 0.4697059977753, 0.783566748644363,
      0.75566986364323, 1.12051953460851, 1.05641521284183, 1.22723625877056,
      1.16768443306172, 3.22379291455248, 5.88219242499701, 8.23891225133386,
      0.361325212075315, 0.346406486960778, 0.804111981127887, 0.70164285854404,
      1.31532906125032, 1.05641521284183, 1.86524285307793, 1.27006709615775,
      1.233791237022,  3.19369557755706, 5.76954297914403,  8.26936423977293,
      0.162383915833626,  0.393770727414352,  0.477170319012822, 0.805150232348093,
      0.855469148134248,  1.22723625877056, 1.27006709615775, 1.61157654495461
    ),
    .Dim = c(12L, 12L),
    .Dimnames = list(
      c(
        "cum_credits_yr1", "cum_credits_yr2",
        "cum_credits_yr3", "cum_credits_yr4",
        "cum_credits_yr1_ela", "cum_credits_yr1_math",
        "cum_credits_yr2_ela", "cum_credits_yr2_math",
        "cum_credits_yr3_ela", "cum_credits_yr3_math",
        "cum_credits_yr4_ela", "cum_credits_yr4_math"
      ),
      c(
        "cum_credits_yr1", "cum_credits_yr2",
        "cum_credits_yr3", "cum_credits_yr4",
        "cum_credits_yr1_ela", "cum_credits_yr1_math",
        "cum_credits_yr2_ela", "cum_credits_yr2_math",
        "cum_credits_yr3_ela", "cum_credits_yr3_math",
        "cum_credits_yr4_ela", "cum_credits_yr4_math"
      )
    )
  )
  mean_struc <- structure(
    c(
      6.14267700354941, 11.8073977209042, 17.6704651597235, 22.9235942462171,
      0.490472632168877, 0.585045768727816, 1.30814496543994, 1.3685316644872,
      2.11105921912946, 2.1423967868485,  2.90808892209976, 2.69517093218756
    ),
    .Names = c(
      "cum_credits_yr1",
      "cum_credits_yr2",
      "cum_credits_yr3",
      "cum_credits_yr4",
      "cum_credits_yr1_ela",
      "cum_credits_yr1_math",
      "cum_credits_yr2_ela",
      "cum_credits_yr2_math",
      "cum_credits_yr3_ela",
      "cum_credits_yr3_math",
      "cum_credits_yr4_ela",
      "cum_credits_yr4_math"
    )
  )

  sim_credits <- mvtnorm::rmvnorm(5000, mean = mean_struc, sigma = cov_matrix)
  sim_credits[, 1] <- recode_credits(sim_credits[, 1], top = 8)
  sim_credits[, 2] <- recode_credits(sim_credits[, 2], top = 16)
  sim_credits[, 3] <- recode_credits(sim_credits[, 3], top = 24)
  sim_credits[, 4] <- recode_credits(sim_credits[, 4], top = 32)
  sim_credits[, 5:12] <- apply(sim_credits[, 5:12], 2, recode_credits)
  sim_credits[, 2] <- ifelse(sim_credits[, 2] > sim_credits[, 1],
                             sim_credits[, 2], sim_credits[, 1] + runif(1, 0, 5))
  sim_credits[, 3] <- ifelse(sim_credits[, 3] > sim_credits[, 2],
                             sim_credits[, 3], sim_credits[, 2] + runif(1, 0, 5))
  sim_credits[, 4] <- ifelse(sim_credits[, 4] > sim_credits[, 3],
                             sim_credits[, 4], sim_credits[, 3] + runif(1, 0, 5))
# Enforce credits not being able to go down year to year
  for(i in c(7, 9, 11)){
    sim_credits[, i] <- ifelse(sim_credits[, i] > sim_credits[, i-2],
                               sim_credits[, i], sim_credits[, i-2] + runif(1, 0, 2))
  }
  for(i in c(8, 10, 12)){
    sim_credits[, i] <- ifelse(sim_credits[, i] > sim_credits[, i-2],
                               sim_credits[, i], sim_credits[, i-2] + runif(1, 0, 2))
  }
  sim_credits <- as.data.frame(sim_credits)


  credit_pattern <- vector(length(gpa_ontrack$gpa), mode = "list")
  TOL <- 2.9162 # sigma from model of gpa on yr4 credits
  for(i in 1:length(gpa_ontrack$gpa)){
    G <- (gpa_ontrack$gpa[[i]] * 2.9464) + 17.7185 # beta from model
    # Fuzzy draw from the total credits vector based on link between credits and GPA
    candidate <- sim_credits[sim_credits[, 4] > G - TOL & sim_credits[, 4] < G + TOL, ]
    credit_pattern[[i]] <- candidate[sample(row.names(candidate), 1),]
  }
  credit_pattern <- bind_rows(credit_pattern)
  credit_pattern <- apply(credit_pattern, 2,  function(x) ceiling(x*2) / 2) #round
  credit_pattern <- as.data.frame(credit_pattern)
  out <- bind_cols(gpa_ontrack, credit_pattern)
  return(out)
}


#' Generate annual GPA sequence for students based on final GPA
#'
#' @param gpa_ontrack a data.frame containing final GPA for a student
#'
#' @return \code{gpa_ontrack} appended with gpa by year
#' @export
gen_annual_gpa <- function(gpa_ontrack){
  cov_matrix <- structure(c(0.90626522250176, 0.695031046675378, 0.530087063133161,
                            0.418621356347014, 0.786058078858986, 0.695031046675378, 0.720702463579063,
                            0.556340603198344, 0.435527907883059, 0.674646530351198, 0.530087063133161,
                            0.556340603198344, 0.561388159783519, 0.437381171701534, 0.544569213486813,
                            0.418621356347014, 0.435527907883059, 0.437381171701534, 0.438273839283868,
                            0.433828410757409, 0.786058078858986, 0.674646530351198, 0.544569213486813,
                            0.433828410757409, 0.824677869865807),
                          .Dim = c(5L, 5L),
                          .Dimnames = list(
                              c("cum_gpa_yr1", "cum_gpa_yr2", "cum_gpa_yr3", "cum_gpa_yr4",
                                "cum_gpa_final"), c("cum_gpa_yr1", "cum_gpa_yr2", "cum_gpa_yr3",
                                                    "cum_gpa_yr4", "cum_gpa_final")))

  mean_struc <-structure(c(2.77892494371257, 2.73927274525083, 2.8066337250337,
                           2.90250269589631, 2.66095257000418),
                         .Names = c("cum_gpa_yr1", "cum_gpa_yr2", "cum_gpa_yr3",
                                    "cum_gpa_yr4", "cum_gpa_final"))
  sim_gpa <- mvtnorm::rmvnorm(5000, mean = mean_struc, sigma = cov_matrix)
  sim_gpa <- as.data.frame(sim_gpa)

  gpa_pattern <- vector(length(gpa_ontrack$gpa), mode = "list")
  TOL <- 0.9 # final GPA SD
  for(i in 1:length(gpa_ontrack$gpa)){
    G <- (gpa_ontrack$gpa[[i]])  # beta from model
    candidate <- sim_gpa[sim_gpa[, 5] > G - TOL & sim_gpa[, 5] < G + TOL, ]
    gpa_pattern[[i]] <- candidate[sample(row.names(candidate), 1),]
  }
  gpa_pattern <- bind_rows(gpa_pattern)
  out <- bind_cols(gpa_ontrack, gpa_pattern)
  return(out)
}


# TODO: Consider logging the first year
#' Calculate on_track based on credits
#'
#' @param gpa_ontrack a data.frame with cumulative credits
#'
#' @return \code{gpa_ontrack} with ontrack indicators appended
#' @export
gen_ontrack <- function(gpa_ontrack){
  # DEFINITION: - on track by end of 9th: 5 total credits, 1 math, 1 English
  # on track by end of 10th: 10 total credits, 1 math, 2  English
  # on track by end of 11th: 15 total credits, 2 math, 3 English
  #on track by end of 12th: 20 total credits, 3 math, 4 English
  gpa_ontrack$ontrack_yr1 <- ifelse(
    (gpa_ontrack$cum_credits_yr1 >= 5 &
      gpa_ontrack$cum_credits_yr1_math >= 1 &
      gpa_ontrack$cum_credits_yr1_ela >= 1), 1, 0)
  gpa_ontrack$ontrack_yr2 <- ifelse(
    gpa_ontrack$cum_credits_yr2 >= 10 &
      gpa_ontrack$cum_credits_yr2_math >= 1 &
      gpa_ontrack$cum_credits_yr2_ela >= 2, 1, 0)
  gpa_ontrack$ontrack_yr3 <- ifelse(
    gpa_ontrack$cum_credits_yr3 >= 15 &
      gpa_ontrack$cum_credits_yr3_math >= 2 &
      gpa_ontrack$cum_credits_yr3_ela >= 3, 1, 0)
  gpa_ontrack$ontrack_yr4 <- ifelse(
    gpa_ontrack$cum_credits_yr4 >= 20 &
      gpa_ontrack$cum_credits_yr4_math >= 3 &
      gpa_ontrack$cum_credits_yr4_ela >= 4, 1, 0)
  return(gpa_ontrack)
}

#' Generate annual HS outcomes
#'
#' @param hs_outcomes a data frame with final gpa, hs_status, and sid
#' @param stu_year a data frame with student enrollment
#' @importFrom tidyr gather
#' @return an expanded table with credits and gpa information
#' @export
gen_hs_annual <- function(hs_outcomes, stu_year){
  gpa_temp <- hs_outcomes[, c("sid", "gpa", "hs_status")]
  gpa_temp <- distinct(gpa_temp, .keep_all=TRUE)

  zzz <- gen_credits(gpa_ontrack = gpa_temp)
  gpa_ontrack <- gen_annual_gpa(gpa_ontrack = zzz)
  gpa_ontrack <- gen_ontrack(gpa_ontrack = gpa_ontrack)
  gpa_ontrack$sid <- as.character(gpa_ontrack$sid)

  ot <- gpa_ontrack %>% select(sid, ontrack_yr1:ontrack_yr4) %>%
    tidyr::gather(key = "yr", value = "ontrack", ontrack_yr1:ontrack_yr4)
  ot$yr <- gsub("ontrack_yr", "", ot$yr)
  ot$yr <- as.numeric(ot$yr)

  cred <- gpa_ontrack %>% select(sid, cum_credits_yr1:cum_credits_yr4) %>%
    tidyr::gather(key = "yr", value = "cum_credits", cum_credits_yr1:cum_credits_yr4)
  cred$yr <- gsub("cum_credits_yr", "", cred$yr)
  cred$yr <- as.numeric(cred$yr)

  credela <- gpa_ontrack %>% select(sid, cum_credits_yr1_ela:cum_credits_yr4_ela) %>%
    tidyr::gather(key = "yr", value = "cum_credits_ela", cum_credits_yr1_ela:cum_credits_yr4_ela)
  credela$yr <- gsub("cum_credits_yr", "", credela$yr)
  credela$yr <- gsub("_ela", "", credela$yr)
  credela$yr <- as.numeric(credela$yr)


  credmath <- gpa_ontrack %>% select(sid, cum_credits_yr1_math:cum_credits_yr4_math) %>%
    tidyr::gather(key = "yr", value = "cum_credits_math", cum_credits_yr1_math:cum_credits_yr4_math)
  credmath$yr <- gsub("cum_credits_yr", "", credmath$yr)
  credmath$yr <- gsub("_math", "", credmath$yr)
  credmath$yr <- as.numeric(credmath$yr)

  gpa <- gpa_ontrack %>% select(sid, cum_gpa_yr1:cum_gpa_yr4) %>%
    tidyr::gather(key = "yr", value = "cum_gpa", cum_gpa_yr1:cum_gpa_yr4)
  gpa$yr <- gsub("cum_gpa_yr", "", gpa$yr)
  gpa$yr <- as.numeric(gpa$yr)

  out <- inner_join(ot, cred, by = c("sid" = "sid", "yr" = "yr"))
  out <- left_join(out, credela, by = c("sid" = "sid", "yr" = "yr"))
  out <- left_join(out, credmath, by = c("sid" = "sid", "yr" = "yr"))
  out <- left_join(out, gpa, by = c("sid" = "sid", "yr" = "yr"))
  rm(ot, cred, credela, credmath, gpa, zzz, gpa_temp)
  out$yr_seq <- out$yr; out$yr <- NULL
  # Only take the first 4 years
  clean_years <- stu_year %>% group_by(sid) %>%
    filter(grade %in% c("9", "10", "11", "12")) %>%
    arrange(year) %>%
    mutate(yr_seq = (year-min(year)) + 1) %>%
    filter(yr_seq < 5) %>% select(-yr_seq) %>%
    select(sid, year , grade, schid)
  gpa_ontrack <- left_join(hs_outcomes, clean_years, by = "sid")
# TODO: check on the year sequence and merging here
  gpa_ontrack <- gpa_ontrack %>% group_by(sid) %>% arrange(sid, year) %>%
    mutate(yr_seq = (year - min(year))+1)
  gpa_ontrack <- left_join(gpa_ontrack, out, by = c("sid" = "sid",
                                                    "yr_seq" = "yr_seq"))
  gpa_ontrack <- gpa_ontrack %>% select(-scale_gpa, -gpa, -grad_prob,
                                        -ps_prob, -ps)

  gpa_ontrack <- gpa_ontrack %>% group_by(sid) %>% arrange(yr_seq) %>%
    mutate(credits_earned = cum_credits - ifelse(!is.na(lag(cum_credits)), lag(cum_credits), 0)) %>%
    mutate(credits_attempted = credits_earned + sample(c(0, 0.25, 0.5, 1, 1.5, 2),
                                                       1, prob = c(0.85, 0.01, 0.04, 0.04, 0.02, 0.01))) %>%
    mutate(cum_credits_attempted = cumsum(credits_attempted)) %>%
    mutate(expected_grad_hs = min(year[grade == "9"]) + 4,
           grad_cohort_ind = ifelse("9" %in% grade, "Yes", "No"))

  tmp_grads <- gpa_ontrack %>% filter(grad == 1) %>% group_by(sid) %>%
    mutate(status_after = ifelse(hs_status == "ontime" & yr_seq < 4, "still_enroll",
                                 NA),
           status_after = ifelse(hs_status == "ontime" & yr_seq == 4, "ontime",
                                 status_after),
           status_after = ifelse(hs_status == "early" & yr_seq < 3, "still_enroll",
                                 status_after),
           status_after = ifelse(hs_status == "early" & yr_seq >= 3, "early",
                                 status_after),
           status_after = ifelse(hs_status == "late" & yr_seq <= 4, "still_enroll",
                                 status_after),
           status_after = ifelse(hs_status == "late" & yr_seq > 4, "late",
                                 status_after))
  eventPool <- list(c(0, 0, 0, 1), c(0, 1, 1, 1), c(0, 0, 1, 1), c(1, 1, 1, 1))

  tmp_nongrads <- gpa_ontrack %>% filter(grad == 0) %>% arrange(sid, year) %>%
    group_by(sid) %>%
    mutate(event = ifelse(hs_status == "transferout", sample(eventPool, 1)[[1]],
                          NA),
           event = ifelse(hs_status == "dropout",
                          sample(eventPool, 1, prob = c(0.1, 0.3, 0.2, 0.4))[[1]],
                          event),
           event = ifelse(hs_status == "disappear", sample(eventPool, 1)[[1]],
                          event),
           event = ifelse(hs_status == "still_enroll", c(0, 0, 0, 0),
                          event)) %>%
    mutate(status_after = ifelse(event == 1, hs_status, "still_enroll"),
           chrt_grad = NA) %>%
    select(-event)
  gpa_ontrack <- bind_rows(tmp_grads, tmp_nongrads)
  return(gpa_ontrack)
}

#' Generate a student-year long table of postsecondary enrollments
#'
#' @param hs_outcomes a dataframe of high school outcomes
#' @param nsc a dataframe of postsecondary institutions
#' @param control a control object from \code{sim_control()}
#' @importFrom tidyr crossing
#' @return a table of enrollments
#' @export
gen_ps_enrollment <- function(hs_outcomes, nsc, control){
  # TODO: Fix cohort here
  ps_pool <- hs_outcomes[hs_outcomes$ps == 1,
                                 c("sid", "ps_prob", "grad", "gpa", "ps",
                                   "chrt_grad")]
  ps_pool$late <- sapply(ps_pool$ps_prob, function(x) rbinom(1, 1, prob = 1-x))
  big <- tidyr::crossing(sid = as.character(unique(ps_pool$sid)), year = 1:4,
                  term = c("fall", "spring"))
  ps_pool <- left_join(big, ps_pool, by = "sid")
  # Add a random chance be a late enroller here
  ps_pool$yr_seq <- ps_pool$year + ps_pool$late
  ps_pool$late <- NULL
  ps_pool$year <- ps_pool$chrt_grad + ps_pool$year # for fall part of school year
  ps_pool$chrt_grad <- NULL
  ps_pool <- ps_pool %>% group_by(sid) %>% arrange(sid, yr_seq, term)
  ps_pool$ps[ps_pool$yr_seq > 1] <- sapply(ps_pool$ps_prob[ps_pool$yr_seq > 1],
                                         function(x) rbinom(1, 1, prob = x))



  ps_pool <- ps_pool %>% group_by(sid) %>% arrange(sid, yr_seq, term) %>%
    mutate(ps_transfer = markov_cond_list("ALL", n = n()-1, control$ps_transfer_list,
                                          t0 = sample(c("0", "1"), 1, prob = c(0.8, 0.2)),
                                          include.t0 = TRUE)
    )
  ps_pool <- ps_pool %>% group_by(sid) %>% arrange(sid, yr_seq, term) %>%
    mutate(opeid = sample(nsc$opeid, 1, prob = nsc$enroll))

  opeid_changer <- function(opeid){
    sample(nsc$opeid[nsc$opeid != opeid],
           1,
           prob =nsc$enroll[nsc$opeid != opeid])
  }
  # Set enrollment year

  ps_pool <- ps_pool %>% group_by(sid) %>% arrange(sid, yr_seq, term) %>%
    mutate(ps_change_ind = cumsum(ifelse(ps_transfer == "1", 1, 0)))
  ps_pool <- ps_pool %>% group_by(sid) %>% arrange(sid, yr_seq, term) %>%
    mutate(opeid = replace(opeid, ps_change_ind == 1, opeid_changer(opeid)),
           opeid = replace(opeid, ps_change_ind == 2, opeid_changer(opeid)),
           opeid = replace(opeid, ps_change_ind == 3, opeid_changer(opeid)),
           opeid = replace(opeid, ps_change_ind == 4, opeid_changer(opeid))) %>%
    select(-ps_change_ind)
  attributes(ps_pool$opeid) <- NULL # Make join work by dropping attributes of IDs
  attributes(nsc$opeid) <- NULL
  ps_pool <- left_join(ps_pool, nsc %>% select(opeid, short_name, type), by = "opeid")
  ps_pool %<>% rename(ps_short_name = short_name,
                      ps_type = type)
  ps_pool %<>% filter(!(yr_seq == 1 & term == "spring"))
  return(ps_pool)
}

