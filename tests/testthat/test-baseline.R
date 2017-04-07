# Test baselines

context("Test baseline extraction")

test_that("ELL baseline works", {
  out <- get_baseline("ell")
  expect_is(out, "list")
  expect_equal(length(out), 3)
  expect_identical(names(out), c("data", "keys", "fun"))
  expect_equal(out$fun(1), 1)
  expect_is(out$data, "data.frame")
  expect_true(all(out$keys %in% names(out$data)))
})

test_that("Error messages are informative", {
  expect_error(get_baseline("iep"), "Baseline not currently defined. Maybe you can write your own?")
})


context("Test assigning baseline values")

ex_data <- data.frame(id = 100:116, age = c(3:19),
                  race = c(rep(c("amerind", "asian", "black", "hispanic",
                               "multiracial", "white", "hawaiian_pi"), 2),
                           "other", "bad", "error"))

ex_data$ell <- assign_baseline(baseline = "ell", data = ex_data)

test_that("Baseline returns the proper object type", {
  zzz <- assign_baseline(baseline = "ell", data = ex_data)
  expect_is(zzz, "integer")
})


test_that("Baseline assignment works for ELL example", {
  expect_true(table(is.na(ex_data$ell))[[2]] == 3)
  expect_true(table(is.na(ex_data$ell))[[1]] == 14)
  expect_true(all(ex_data$ell %in% c(0L, 1L, NA)))
})

# Check for error reporting


ex_data <- data.frame(id = 100:116, age = c(3:19),
                      race = c(rep(c("amerind", "asian", "black", "hispanic",
                                     "multiracial", "white", "hawaiian_pi"), 2),
                               "other", "bad", "error"))

test_that("Baseline returns the proper object type for program", {
  zzz <- assign_baseline(baseline = "program", data = ex_data)
  expect_is(zzz, "data.frame")
  expect_identical(names(zzz),
                   c("id", "age", "race", "ell", "iep", "frpl"))
})

test_that("Baseline assignment works for program example", {
  zzz <- assign_baseline(baseline = "program", data = ex_data)
  expect_true(all(zzz$ell %in% c(0L, 1L)))
  expect_true(all(zzz$iep %in% c(0L, 1L)))
  expect_true(all(zzz$frpl %in% c(0L, 1L)))
})

test_that("Baseline for program is well defined", {
  bl <- get_baseline("program")
  expect_is(bl, "list")
  expect_identical(names(bl), c("data", "keys", "fun"))
  expect_is(bl$data, "data.frame")
  expect_null(bl$keys)
  expect_is(bl$fun, "function")
  out <- bl$fun()
  expect_is(out, "data.frame")
  expect_identical(names(out), c("ell", "iep", "frpl"))
})

# grade baseline
test_that("Baseline returns the proper object type for grade", {
  zzz <- assign_baseline("grade", ex_data)
  expect_is(zzz, "data.frame")
  expect_identical(names(zzz),
                   c("id", "age", "race", "grade"))
})

test_that("Baseline assignment works for program example", {
  zzz <- assign_baseline("grade", ex_data)
  expect_is(zzz$grade, "character")
  expect_true(all(unique(zzz$grade) %in%
                    c("1", "10", "11", "12", "2", "3", "4", "5", "6",
                      "7", "8", "9", "KG", "PK")))
})

test_that("Baseline for grade is well defined", {
  bl <- get_baseline("grade")
  expect_is(bl, "list")
  expect_identical(names(bl), c("data", "keys", "fun"))
  expect_is(bl$data, "data.frame")
  expect_identical(bl$keys, "age")
  expect_is(bl$fun, "function")
  out <- bl$fun(2)
  expect_is(out, "character")
})

# cond_prob
ses_list <- list(
  "white" = list(f = runif,
                 pars = list(min = 10, max = 20)),
  "hispanic" = list(f = runif,
                    pars = list(min = 25, max = 30)),
  "black" = list(f = runif,
                 pars = list(min = 35, max = 40)),
  "asian" = list(f = runif,
                 pars = list(min = 45, max = 50)),
  "multiracial" = list(f = runif,
                       pars = list(min = 100, max = 105)),
  "amerind" = list(f = runif,
                   pars = list(min = 0, max = 5)),
  "other" = list(f = runif,
                 pars = list(min = -10, max = -5)),
  "hawaiian_pi" = list(f = runif,
                       pars = list(min = -20, max = -15))
)


context("Test conditional probability")

test_that("warnings but functioning", {
  test_1 <- ex_data[1:5, ]
  expect_warning(cond_prob(test_1, factor = "race",
                   newvar = "ses", prob_list = ses_list))
  zzz <- cond_prob(test_1, factor = "race",
                   newvar = "ses", prob_list = ses_list)
  expect_is(zzz, "data.frame")
  expect_identical(names(zzz), c("id", "age", "race", "ses"))
  expect_identical(zzz[, 1:3], ex_data[1:5, ])
  expect_gt(zzz$ses[zzz$race == "amerind"], 0)
  expect_lt(zzz$ses[zzz$race == "amerind"], 5)
  expect_gt(zzz$ses[zzz$race == "asian"], 45)
  expect_lt(zzz$ses[zzz$race == "asian"], 50)
  expect_gt(zzz$ses[zzz$race == "black"], 35)
  expect_lt(zzz$ses[zzz$race == "black"], 40)
  expect_gt(zzz$ses[zzz$race == "hispanic"], 25)
  expect_lt(zzz$ses[zzz$race == "hispanic"], 30)
  expect_gt(zzz$ses[zzz$race == "multiracial"], 100)
  expect_lt(zzz$ses[zzz$race == "multiracial"], 105)
})


## Test ses baseline
## Test baseline assignment and conditional probability
## Test school assignment
## Test school transition assignment


