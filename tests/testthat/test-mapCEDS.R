

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

test_that("Errors make sense when column names are mixed", {
  mixNames <- c("Birthdate", "white", "asian", "black", "hispanic",
                "Primary Disability Type")
  expect_message(map_CEDS(mixNames), "Not all names successfully matched.", fixed = TRUE)
  expect_message(map_CEDS(mixNames), "Returning NA for those not matched.", fixed = TRUE)
  expect_message(map_CEDS(mixNames), "Majority of names match CEDS names", fixed = TRUE)
  expect_message(map_CEDS(mixNames), "returning CEDS names", fixed = TRUE)
  expect_message(map_CEDS(mixNames), "Both SDP and CEDS names detected.", fixed = TRUE)
  cleaned <- map_CEDS(mixNames)
  expect_equal(length(cleaned[is.na(cleaned)]), 2)
  expect_equal(length(cleaned[!is.na(cleaned)]), 4)
  expect_identical(cleaned[!is.na(cleaned)], c("White", "Asian", "Black or African American",
                                               "Hispanic or Latino Ethnicity"))
  mixNames <- c("Birthdate", "White", "Asian", "black", "hispanic",
                "Primary Disability Type")
  expect_message(map_CEDS(mixNames), "Not all names successfully matched.", fixed = TRUE)
  expect_message(map_CEDS(mixNames), "Returning NA for those not matched.", fixed = TRUE)
  expect_message(map_CEDS(mixNames), "Majority of names match SDP names", fixed = TRUE)
  expect_message(map_CEDS(mixNames), "returning SDP names", fixed = TRUE)
  expect_message(map_CEDS(mixNames), "Both SDP and CEDS names detected.", fixed = TRUE)
  cleaned <- map_CEDS(mixNames)
  expect_equal(length(cleaned[is.na(cleaned)]), 2)
  expect_equal(length(cleaned[!is.na(cleaned)]), 4)
  expect_identical(cleaned[!is.na(cleaned)], c("dob", "white", "asian", "disab_type"))
})

test_that("get_code_values extracts values", {
  test_string <- c("Yes - Yes;No - No;NotSelected - Not Selected")
  codes <- get_code_values(test_string)
  expect_equal(length(codes), 2)
  expect_identical(names(codes), c("values", "labels"))
  expect_null(names(codes$values))
  expect_null(names(codes$labels))
  expect_equal(length(codes$values), 3)
  expect_equal(length(codes$labels), 3)
  expect_identical(codes$labels, c("Yes", "No", "Not Selected"))
  expect_identical(codes$values, c("Yes", "No", "NotSelected"))
  bad_string <- c("Yes - Yes,No - No,NotSelected - Not Selected")
  codes <- get_code_values(bad_string)

})

test_that("make_inds makes indicator variables", {
  testDat <- data.frame(id = 1:30,
                        group = sample(letters[1:5], 30, replace=TRUE))
  outDat <- make_inds(testDat, col = "group")
  expect_true(ncol(outDat) > ncol(testDat))
  expect_equal(ncol(outDat), 7)
  expect_equal(nrow(testDat), nrow(outDat))
  expect_identical(names(outDat)[3:7], letters[1:5])
  expect_identical(names(outDat)[1:2], names(testDat)[1:2])
})
#
# test_that("recode_options successfully recodes", {
#
#
#
# })

# recode_options
