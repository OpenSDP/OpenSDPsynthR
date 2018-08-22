# age, retention, and moves calculations
# From eeptools package
context("Test age calculator")

test_that("Leap year calculations work", {
  # from @larmarange
  expect_equal(age_calc(as.Date('2004-01-15'), as.Date('2004-02-16')), 1.034483,
               tol = .00001)
  expect_equal(age_calc(as.Date('2005-01-15'), as.Date('2005-02-16')), 1.035714,
               tol = .00001)
  expect_equal(age_calc(as.Date('1995-01-15'), as.Date('2003-02-16')),
               age_calc(as.Date('1994-01-15'), as.Date('2002-02-16')))
  expect_false(age_calc(as.Date('1996-01-15'), as.Date('2004-02-16')) ==
                 age_calc(as.Date('1994-01-15'), as.Date('2002-02-16')))
})

test_that("All function parameters result in a numeric calculations with sane inputs", {
  tests <- expand.grid(precise = c(TRUE, FALSE),
                       units = c("days", "months", "years"),
                       dob = c("atomic", "vector"),
                       enddate = c("atomic", "vector"))

  safe.ifelse <- function(cond, yes, no) structure(ifelse(cond, yes, no), class = class(yes))

  for(i in 1:nrow(tests)){
    atomDOB <- as.Date(as.POSIXct('1987-05-29 018:07:00'))
    vecDOB <- as.Date(seq(as.POSIXct('1987-05-29 018:07:00'), len=26, by="21 day"))
    vecED <- as.Date(seq(as.POSIXct('2017-05-29 018:07:00'), len=26, by="21 day"))
    atomED <- as.Date(as.POSIXct('2017-05-29 018:07:00'))

    dob <- safe.ifelse(tests[i, "dob"] == "atomic", atomDOB, vecDOB)
    enddate <- safe.ifelse(tests[i, "enddate"] == "atomic", atomED, vecED)

    out <- age_calc(dob = dob, enddate = enddate, units = tests[i, ]$units,
                    precise = tests[i, ]$precise)
    expect_true(class(out) %in% c("difftime", "numeric"))
  }

})

test_that("Bad inputs yield correct errors", {
  expect_error(age_calc('2004-01-15', '2004-02-16'),
               "Both dob and enddate must be Date class objects")
  expect_error(age_calc(as.Date('2004-01-15'), '2004-02-16'),
               "Both dob and enddate must be Date class objects")
  expect_error(age_calc('2004-01-15', as.Date('2004-02-16')),
               "Both dob and enddate must be Date class objects")
  expect_error(age_calc(as.Date('2004-02-16'), as.Date('2004-01-15')),
               "End date must be a date after date of birth")

})
