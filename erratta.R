library(simglm)

fixed <- ~1 + time
random <- ~1 + time + gifted.f + iep.f + frpl.f + ell.f
random3 <- ~ 1
cor_vars <- NULL # needs to be the length of all correlations between predictors
fixed_param <- c(100, 25, 10)
fact_vars <- list(numlevels = c(2, 2, 2, 2), var_type = c(rep('lvl2', 4)))
random_param <- list(random_var = c(50, 90, 10, 10, 10, 10),
                     # cor_vars = c(0.4, 0.2, 0.2, 0.2, 0.2, 0.2),
                     rand_gen = "rnorm") # intercept + any slopes in length
random_param3 <- list(random_var = c(200), rand_gen = 'rnorm') # intercept + any slopes in length
unbalCont <- c(2, 16)
unbalCont3 <- c(100, 800)
# Total number of level 2 groups = k * n
k <- 15 # level 3 groups
n <- 200 # obs per group level 2 group
p <- 400 # obs per group?
error_var <- 10
with_err_gen <- 'rnorm'
lvl1_err_params <- list(mean = 20, sd = 10)
data_str <- "cross"
cov_param <- NULL
temp_three <- sim_reg(fixed = fixed, random = random, random3 = random3,
                      fixed_param = fixed_param, random_param = random_param,
                      random_param3 = random_param3, cov_param = cov_param,
                      fact_vars = fact_vars, k = k,n = n, p = p,
                      lvl1_err_params = lvl1_err_params,
                      error_var= error_var, with_err_gen = with_err_gen,
                      cor_vars = cor_vars, data_str = "cross", unbal = TRUE,
                      unbalCont = unbalCont, unbalCont3 = unbalCont3)

