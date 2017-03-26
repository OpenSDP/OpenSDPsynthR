

# map_CEDS
# recode_options

# Gen fake data

context("Test CEDS variable mapping")

test_that("Error is appropriate when supplying dataframe...", {
  SDP <- data.frame("sid" = NA, "ell" = NA, "iep" = NA, "frpl" = NA)
  expect_error(map_CEDS(SDP), "passing or reassigning names(object)", fixed=TRUE)
  SDP <- as.tbl(SDP)
  expect_error(map_CEDS(SDP), "Please supply a character vector of names to be converted.", fixed=TRUE)
})

test_that("Check names return dimensions", {
  allSDPnames <- c("dob", "white", "asian", "amerind", "black", "hispanic",
                   "multiracial", "hawaiian_pi", "other", "ell", "disab_type",
                   "gifted", "frpl", "iep", "sid", "sex", "age")
  cleaned <- map_CEDS(allSDPnames)
  expect_equal(length(allSDPnames), length(cleaned))
  allCEDSnames <- c("Birthdate", "White", "Asian", "American Indian or Alaska Native",
                    "Black or African American", "Hispanic or Latino Ethnicity",
                    "Demographic Race Two or More Races",
                    "Native Hawaiian or Other Pacific Islander",
                    "", "Limited English Proficiency Status",
                    "Primary Disability Type",
                    "Gifted and Talented Indicator", "Economic Disadvantage Status",
                    "IDEA Indicator", "Student Identifier", "Sex")
  cleaned <- map_CEDS(allCEDSnames)
  expect_equal(length(allCEDSnames), length(cleaned))
})


# test error when names are mixed CEDS and SDP
# test result when names are neither CEDS or SDP
# test dimensions returned

