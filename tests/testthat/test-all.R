# All tests

# expand.grid.df

context("Test expand_grid_df")
ex_data <- data.frame(id = 100:116, age = c(3:19),
                      race = c(rep(c("amerind", "asian", "black", "hispanic",
                                     "multiracial", "white", "hawaiian_pi"), 2),
                               "other", "bad", "error"))

test_that("Expand grid returns expected objects", {
  out <- expand_grid_df(ex_data, data.frame(year = 2006:2012))
  expect_is(out, "data.frame")
  expect_equal(ncol(out), 4)
  expect_identical(names(out)[4], "year")
  expect_equal(nrow(out), nrow(ex_data)*7)
  expect_identical(out[1:5, 1:3], ex_data[1:5, 1:3])
  out <- expand_grid_df(ex_data, c("year" = 2006:2012))
  expect_is(out, "data.frame")
  expect_equal(ncol(out), 4)
  expect_identical(names(out)[4], "y")
  expect_equal(nrow(out), nrow(ex_data) * 7)
  expect_identical(out[1:5, 1:3], ex_data[1:5, 1:3])
  out <- expand_grid_df(ex_data, 2006:2012)
  expect_is(out, "data.frame")
  expect_equal(ncol(out), 4)
  expect_identical(names(out)[4], "y")
  expect_equal(nrow(out), nrow(ex_data) * 7)
  expect_identical(out[1:5, 1:3], ex_data[1:5, 1:3])
})


# assign_grade

# cond_prob

# createSeries


# findTransitions

# get_baseline

# get_code_values

# ell

# xwalk
