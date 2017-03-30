library(simglm)






out <- zed2 %>% group_by(clustID) %>%
  summarize(count = n(), unique = length(unique(withinID)))



# sim graduation
fixed <- ~ 1 + gifted.f + iep.f + frpl.f + ell.f + male.f
random <- ~ 1
cor_vars <- NULL # needs to be the length of all correlations between predictors
fixed_param <- c(1.06, 0.72, -0.20, -0.513, -0.4669, -0.356)
fact_vars <- list(numlevels = c(2, 2, 2, 2, 2), var_type = c(rep('lvl1', 5)))
random_param <- list(random_var = c(0.7728), rand_gen = 'rnorm') # intercept + any slopes in length
unbalCont <- c(100, 600)
# Total number of level 2 groups = k * n
n <- 15 # obs per group level 2 group
#p <- 400 # obs per group?
data_str <- "long"
cov_param <- NULL
grad_df <- sim_glm(fixed = fixed, random = random,
                      fixed_param = fixed_param, random_param = random_param,
                      random3 = NULL,
                      random_param3 = NULL,
                      cov_param = cov_param,
                      fact_vars = fact_vars, k = NULL,
                      n = n, p = p,
                      cor_vars = cor_vars, data_str = "cross", unbal = TRUE,
                      unbalCont = unbalCont)




# simglm - longitudinal assessment
# Three level example
## TODO - make groupings unbalanced
## TODO - add factor variables
## TODO - tune parameters to get output desired
fixed <- ~1 + time + gifted.f + iep.f + frpl.f + ell.f
random <- ~1 + time
random3 <- ~ 1
cor_vars <- NULL # needs to be the length of all correlations between predictors
fixed_param <- c(100, 25, 3, -4, -3, -1)
fact_vars <- list(numlevels = c(2, 2, 2, 2), var_type = c(rep('lvl1', 4)))
random_param <- list(random_var = c(50, 90), cor_vars = c(0.4), rand_gen = 'rnorm') # intercept + any slopes in length
random_param3 <- list(random_var = c(200), rand_gen = 'rnorm') # intercept + any slopes in length
# cov_param <- list(dist_fun = c('rnorm', 'rnorm','rnorm'), # need npar - 1 for lvl1 elements
#                   var_type = c("lvl1", "lvl2", "lvl3"),
#                   opts = list(
#                     list(mean = 0, sd = 1.5),
#                     #list(mean = 1, sd = 3),
#                     list(mean = 0, sd = 4),
#                     list(mean = 0, sd = 2)))
#
unbalCont <- c(2, 16)
unbalCont3 <- c(100, 800)
# Total number of level 2 groups = k * n
k <- 15 # level 3 groups
n <- 200 # obs per group level 2 group
p <- 400 # obs per group?
error_var <- 10
with_err_gen <- 'rnorm'
lvl1_err_params <- list(mean = 20, sd = 10)
data_str <- "long"
cov_param <- NULL
temp_three <- sim_reg(fixed = fixed, random = random, random3 = random3,
                      fixed_param = fixed_param, random_param = random_param,
                      random_param3 = random_param3, cov_param = cov_param,
                      fact_vars = fact_vars, k = k,n = n, p = p,
                      lvl1_err_params = lvl1_err_params,
                      error_var= error_var, with_err_gen = with_err_gen,
                      cor_vars = cor_vars, data_str = "long", unbal = TRUE,
                      unbalCont = unbalCont, unbalCont3 = unbalCont3)

library(ggplot2)

ggplot(temp_three, aes(x = time, y = sim_data, group = clustID)) +
  geom_line(alpha = I(0.2)) + facet_wrap(~clust3ID)

names(temp_three)[1:6] <- c("intercept", "age", "gifted", "iep", "frpl",
                       "ell")
names(temp_three)[14] <- "math_ss"
names(temp_three)[15:17] <- c("time", "sid", "schid")

ggplot(temp_three, aes(x = age, y = math_ss, group = sid)) +
  geom_line(alpha = I(0.2)) + facet_wrap(~schid)

#witihnID = time, nested w/in level 2

library(lme4)
proof <- lmer(math_ss ~ 1 + age + gifted +
                iep + frpl + ell +
                (1 + age | sid) +
                (1 | schid), data = temp_three)

testdf <- expand.grid(frpl = 0:1, ell = 0:1,
                      iep = 0:1, gifted = 0:1, schid = 1:15)
testdf$sid <- 1:nrow(testdf)
testdf <- expand_grid_df(testdf, list("age" = 4:15))
testdf$yhat <- predict(proof, newdata = testdf, re.form = ~(1|schid))
testdf$yhat <- predict(proof, newdata = testdf)
zed <- simulate(proof, nsim = 100, newdata = testdf,
                re.form = ~(1|schid))
testdf$yhat <- apply(zed, 1, function(x) sample(x, 1))

ggplot(testdf, aes(x = age, y = yhat, group = sid)) +
  geom_line(alpha = I(0.2)) + facet_wrap(~schid)

head(out$stu_year)



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
