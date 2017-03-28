
# simglm

library(simglm)

# Three level example
## TODO - make groupings unbalanced
## TODO - add factor variables
## TODO - tune parameters to get output desired
fixed <- ~1 + time + math + rdg + frpl + ell.f + time:math
random <- ~1 + time
random3 <- ~ 1
cor_vars <- c(0.7, 0.4, 0.2) # needs to be the length of all correlations between predictors
fixed_param <- c(4, 6, 2.3, 7, 4, 2, 0)
fact_vars <- list(numlevels = c(2), var_type = c('lvl1'))
random_param <- list(random_var = c(4, 2), rand_gen = 'rnorm') # intercept + any slopes in length
random_param3 <- list(random_var = c(4), rand_gen = 'rnorm') # intercept + any slopes in length
cov_param <- list(dist_fun = c('rnorm', 'rnorm', 'rnorm'), # need npar - 1 for lvl1 elements
                  var_type = c("lvl1", "lvl2", "lvl3"),
                  opts = list(list(mean = 0, sd = 1.5),
                              #list(mean = 1, sd = 3),
                              list(mean = 0, sd = 4),
                              list(mean = 0, sd = 2)))
unbalCont <- c(2, 16)
unbalCont3 <- c(100, 800)
# Total number of level 2 groups = k * n
k <- 15 # level 3 groups
n <- 20 # obs per group?
p <- 200 # obs per group?
error_var <- 4
with_err_gen <- 'rnorm'
data_str <- "long"
temp_three <- sim_reg(fixed = fixed, random = random, random3 = random3,
                      fixed_param = fixed_param, random_param = random_param,
                      random_param3 = random_param3, cov_param = cov_param,
                      fact_vars = fact_vars, k = k,n = n, p = p,
                      error_var= error_var, with_err_gen = with_err_gen,
                      cor_vars = cor_vars, data_str = "long", unbal = TRUE,
                      unbalCont = unbalCont, unbalCont3 = unbalCont3)

#witihnID = time, nested w/in level 2

library(lme4)
proof <- lmer(sim_data ~ 1 + time + diff + math + rdg + time:math +
       (1 + time + diff | clustID) +
       (1 | clust3ID), data = temp_three)

##########
out <- simpop(500, seed = 3522)
##########

fixed <- ~ 1 + act.o + diff.o + numCourse.o + fred.f + act.o:numCourse.o
fixed_param <- c(0.8, 1, 0.2, 0.1, 2, 1, 3)
cov_param <- NULL
fact_vars <- list(numlevels = c(36, 8, 5, 3), var_type = c('single', 'single', 'single', "single"))
n <- 150
error_var <- 3
with_err_gen = 'rnorm'
temp_single_o <- sim_reg(fixed = fixed, fixed_param = fixed_param,
                         cov_param = cov_param, n = n, error_var = error_var,
                         with_err_gen = with_err_gen, data_str = "single",
                         fact_vars = fact_vars)



#################
# frpl
tm_f <- matrix(c(900, 200, 300, 2000), nrow = 2, byrow=TRUE,
               dimnames = list(statesNames, statesNames))
make_markov_series(20, tm = tm_f/rowSums(tm_f))

# gifted
tm_g <- matrix(c(2000, 200, 20, 400), nrow = 2, byrow=TRUE,
               dimnames = list(statesNames, statesNames))
make_markov_series(20, tm = tm_g/rowSums(tm_g))
# iep
tm_i <- matrix(c(3000, 200, 200, 3000), nrow = 2, byrow=TRUE,
               dimnames = list(statesNames, statesNames))
make_markov_series(20, tm = tm_i/rowSums(tm_i))


make_markov_series(20, tm = tm_grade/rowSums(tm_grade), t0 = "1")



stu_year %<>% group_by(ID) %>% arrange(ID, year) %>%
  mutate(frpl = make_markov_series(n(), tm = tm_f/rowSums(tm_f)),
         gifted = make_markov_series(n(), tm = tm_g/rowSums(tm_g)),
         iep = make_markov_series(n(), tm = tm_i/rowSums(tm_i)),
         grade_adv = make_markov_series(n(), tm = tm_grade/rowSums(tm_grade)))
