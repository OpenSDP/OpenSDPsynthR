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

test_that("Baseline assignment works for ELL example", {
  expect_true(table(is.na(ex_data$ell))[[2]] == 3)
  expect_true(table(is.na(ex_data$ell))[[1]] == 14)
  expect_true(all(ex_data$ell %in% c(0L, 1L, NA)))
})

# Check for error reporting

ex_data <- data.frame(id = 100:116, age_year = c(3:19),
                      race_eth = c(rep(c("amerind", "asian", "black", "hispanic",
                                     "multiracial", "white", "hawaiian_pi"), 2),
                               "other", "bad", "error"))


test_that("Baseline assignment errors are informative", {
  expect_error(assign_baseline(baseline = "ell", data = ex_data),
               "Data supplied does not have right keys to merge")
})

## Test program baseline
## Test grade baseline
## Test ses baseline
## Test baseline assignment and conditional probability
## Test school assignment
## Test school transition assignment


