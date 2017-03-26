# Test gen

context("Test high level data generation functions")

#TODO make gen_students produce consistent number of columns
test_that("gen_students produces students", {
  out <- gen_students(n = 10, seed = 21421)
  expect_equal(nrow(out), 10)
  expect_is(out, "data.frame")
  expect_identical(names(out),
                   c("sid", "Sex", "Birthdate", "Race", "White",
                     "Black.or.African.American"))
  expect_is(out[, "sid"], "factor")
  expect_is(out[, "Sex"], "factor")
  expect_is(out[, "Birthdate"], "Date")
  expect_is(out[, "Race"], "factor")
  expect_is(out[, "White"], "character")
  expect_is(out[, "Black.or.African.American"], "character")
  out <- gen_students(n = 100, seed = 21421)
  expect_equal(nrow(out), 100)
  expect_equal(ncol(out), 10)
})

test_that("gen_student_years longitudinal data", {
  out <- gen_students(n = 100, seed = 2352)
  out_l <- gen_student_years(data = out)
  expect_message(gen_student_years(data = out), regexp = NA)
  expect_equal(ncol(out_l), 4)
  # Figure out how to test number of rows produced
  expect_equal(min(out_l$year), sim_control()$minyear)
  expect_equal(max(out_l$year), sim_control()$maxyear)
  expect_identical(names(out_l), c("sid", "year", "Birthdate", "age"))
})

# gen_annual_status
# gen_initial_status
# simpop
# gen_student_years
# make_inds
# sim_control
