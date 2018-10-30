
sim_par <- sim_control()$assess_sim_par

# TODO: Figure out how to sample levels correctly for each factor
# TODO: Figure out how to align regression coefficients
# TODO: Correlation among covariates should be incorporated somehow
# TODO: Unbalanced design data?

sim_arguments <- list(
  formula = update(sim_par$fixed, y ~ . + (1+time|id) + (1|schid)),
  reg_weights = sim_par$fixed_param,
  fixed = list(
    time = list(var_type = 'time', time_levels = 1:sim_par$k),
    gifted = list(var_type = 'factor', levels = c(0, 1)),
    iep = list(var_type = 'factor', levels = c(0, 1)),
    frpl = list(var_type = 'factor', levels = c(0, 1, 2)),
    ell = list(var_type = 'factor', levels = c(0, 1)),
    male = list(var_type = 'factor', levels = c(0, 1))
  ),
  # outcome_type = 'binary',
  randomeffect = list(int_id = list(variance = 0.5, var_level = 2),
                      time_id = list(variance = 0.125, var_level = 2),
                      schid = list(variance = 0.1, var_level = 3)),
  sample_size = list(level1 = sim_par$k, level2 = sim_par$n,
                     level3 = sim_par$k),
  error = list(dist = "rnorm",
               variance = sim_par$lvl1_err_params$sd^2)
)

error_data <- simulate_error(data = NULL, sim_arguments)

zzz <- simulate_fixed(data = NULL, sim_arguments)

zzz <- simulate_fixed(data = NULL, sim_arguments) %>%
  simulate_randomeffect(sim_arguments) %>%
  simulate_error(sim_arguments) %>%
  generate_response(sim_arguments)


###################################################
###########################################################
