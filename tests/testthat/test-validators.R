# Test validation tools


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


#

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

assess_sim_par <- OpenSDPsynthR::sim_control()$assess_sim_par
gpa_sim_par <- OpenSDPsynthR::sim_control()$gpa_sim_par

test_that("Validators work for two level and three level sim", {
  expect_true(validate_sim_parameter(assess_sim_par))
  expect_true(validate_sim_parameter(gpa_sim_par))
  expect_true(validate_sim_parameter(grad_sim_parameters))
})

>>>>>>> 99f079c02afe3294a4bddd8cce3fa2bcbc7be254
