# Test gen

context("Test high level data generation functions")

#TODO make gen_students produce consistent number of columns
test_that("gen_students produces students", {
  set.seed(21421)
  out <- gen_students(n = 10)
  expect_equal(nrow(out), 10)
  expect_is(out, "data.frame")
  expect_identical(names(out),
                   c("sid", "Sex", "Birthdate", "Race", "White",
                     "Black.or.African.American",
                     "Hispanic.or.Latino.Ethnicity",
                     "Demographic.Race.Two.or.More.Races", "id_type"))
  expect_is(out[, "sid"], "factor")
  expect_is(out[, "Sex"], "factor")
  expect_is(out[, "Birthdate"], "Date")
  expect_is(out[, "Race"], "factor")
  expect_is(out[, "White"], "character")
  expect_is(out[, "Black.or.African.American"], "character")
  out <- gen_students(n = 100)
  expect_equal(nrow(out), 100)
  expect_equal(ncol(out), 11)
})

test_that("gen_student_years longitudinal data", {
  set.seed(2352)
  out <- gen_students(n = 100)
  out_l <- gen_student_years(data = out)
  expect_message(gen_student_years(data = out), regexp = NA)
  expect_equal(ncol(out_l), 8)
  # Figure out how to test number of rows produced
  expect_equal(min(out_l$year), sim_control()$minyear)
  expect_equal(max(out_l$year), sim_control()$maxyear)
  expect_identical(names(out_l)[1:4], c("sid", "year", "Birthdate", "age"))
  expect_identical(names(out_l)[5:8], c("enrollment_status", "cohort_grad_year",
                                        "cohort_year", "exit_type"))
})


# context("Test annual status variables")
# ex_data <- data.frame(id = 100:116,
#                       Sex = sample(c("Male", "Female"), 17, replace = TRUE),
#                       Race = sample(c("White", "Asian"), 17, replace = TRUE))
#
# gen_annual_status(ex_data)
#
# test_that("Does it work?", {
#
# })
#
# cond_vars <- get_sim_groupvars(control)
# stu_year <- left_join(stu_year, demog_master[, c(idvar, cond_vars)],
#                       by = idvar)
# stu_year <- gen_annual_status(stu_year, control = control)


# gen_annual_status
# gen_initial_status
# simpop
# gen_student_years
# make_inds
# sim_control

test_that("Simulation control works", {
  expect_is(sim_control(), "list")

})
