#' Convert a population simulation into an analysis file
#'
#' @param simouts result of the \code{simpop} function
#'
#' @return an analysis file as a single dataframe for HS outcomes
#' @import dplyr
#' @export
sdp_cleaner <- function(simouts){
  hs_summary <- simouts$stu_year %>%
    filter(grade %in% c("9", "10", "11", "12")) %>%
    group_by(sid) %>% arrange(sid, year) %>%
    summarize(
      first_hs_code = first(schid),
      last_hs_code = last(schid),
      frpl_ever_hs = ifelse(any(frpl == "1"), "1", "0"),
      iep_ever_hs = ifelse(any(iep == "1"), "1", "0"),
      ell_ever_hs = ifelse(any(ell == "1"), "1", "0"),
      gifted_ever_hs = ifelse(any(gifted == "1"), "1", "0"),
      chrt_ninth = min(year[grade == "9"]),
      chrt_grad = min(year[grade == "12"]),
      nyears = n()
    )

  scores <- simouts$stu_assess %>% ungroup %>%
    filter(grade %in% c("8")) %>%
    mutate(
      test_math_8_raw = math_ss,
      test_math_8_std =  rescale(math_ss),
      qrt_8_math = ntile(math_ss, 4),
      test_ela_8_raw = rdg_ss,
      test_ela_8_std = rescale(rdg_ss),
      qrt_8_ela = ntile(rdg_ss, 4),
      test_composite_8 = math_ss + rdg_ss,
      test_composite_8_std = test_math_8_std + test_ela_8_std,
      qrt_8_composite = ntile(test_composite_8, 4)
    )

  credits_long <- na.omit(na.omit(as.data.frame(simouts$hs_annual[, c(1, 7, 8:12)])))
  credits_wide <- reshape(credits_long,
                          timevar = "yr_seq",
                          sep = "_yr",
                          idvar = "sid",
                          direction = "wide")
  rm(credits_long)

  outcomes_clean <- bind_rows(
    simouts$hs_outcomes %>% group_by(sid) %>%
      mutate(nrow = n()) %>%
      ungroup %>% arrange(sid) %>%
      filter(nrow > 1) %>% group_by(sid) %>%
      mutate(keep = ifelse(scale_gpa == min(scale_gpa), 1, 0)) %>%
      filter(keep == 1) %>%
      select(-nrow, -keep),
    simouts$hs_outcomes %>% group_by(sid) %>%
      mutate(nrow = n()) %>%
      ungroup %>% arrange(sid) %>%
      filter(nrow == 1) %>% select(-nrow)
  )

  outcomes_wide <- ungroup(outcomes_clean) %>% make_inds("hs_status")
  outcomes_wide <- as.data.frame(outcomes_wide)
  outcomes_wide <- outcomes_wide %>% select(-scale_gpa, -gpa, -grad_prob,
                                            -grad, -hs_status, -ps_prob, -ps)

  ps_long <- simouts$ps_enroll
  ps_long <- ps_long[, c("sid", "year", "term", "ps", "opeid")]
  ps_long <- ps_long %>% group_by(sid, year) %>%
    summarize(
      enroll_any = ifelse(any(ps > 0), 1, 0),
      enroll_full = ifelse(all(ps > 0), 1, 0),
      nschls = length(unique(opeid)),
      first_ps = first(opeid),
      last_ps = last(opeid)
    ) %>%
    mutate(
      no_trsfr = ifelse(first(first_ps) == first(last_ps), 1, 0)
    )
  ps_long <- as.data.frame(ps_long)

  ps_wide <- reshape(ps_long[, c("sid", "year", "enroll_any", "enroll_full")],
                     timevar = "year",
                     sep = "_yr",
                     idvar = "sid",
                     direction = "wide")

  demog_clean <- simouts$demog_master %>%
    group_by(sid) %>%
    summarize(
      male = if_else(Sex == "Male", 1, 0),
      race_ethnicity = Race
    )
  final_data <- left_join(demog_clean, hs_summary, by = "sid")
  final_data <- left_join(final_data, outcomes_wide, by = "sid")
  final_data <- left_join(final_data, outcomes_clean, by = "sid")
  final_data <- left_join(final_data, ps_wide, by = "sid")
  final_data <- left_join(final_data, scores, by = "sid")
  final_data <- left_join(final_data, credits_wide, by = "sid")
  return(final_data)
}

## Postsecondary wide

# TODO - check why ps and ps_yr1-ps_yr4 are not connected

# Figure out status indicators

#
# stuOT %<>% group_by(sid, year_in_hs) %>%
#   mutate(status_eoy = ifelse(ontrack_endyr == 1, 1, 2)) %>%
#   mutate(status_eoy = ifelse(dropout == 1, 3, status_eoy)) %>%
#   mutate(status_eoy = ifelse(disappear == 1, 4, status_eoy)) %>%
#   ungroup()
# sum(table(stuOT$status_eoy)) == nrow(stuOT)
# [1] TRUE
# 2.3b Now, define status after 4th year using diploma information.
# tmp <- stuOT %>% filter(year_in_hs == 4) %>%
#   group_by(sid) %>%
#   mutate(status_eoy_yr4 = ifelse(ontime_grad == 1 & !is.na(chrt_grad), 1, 0)) %>%
#   mutate(status_eoy_yr4 = ifelse(still_enrl == 1 | late_grad ==1, 2, status_eoy)) %>%
#   SDP TOOLKIT FOR EFFECTIVE DATA USE | COLLEGE GOING | CONNECT 35
# Step 3: Generate GPA and Test variables CONNECT: ON TRACK INDICATORS
# mutate(status_eoy_yr4 = ifelse(is.na(hs_diploma_date) & is.na(status_eoy) &
#                                  disappear == 1, 4, status_eoy)) %>%
#   ungroup() %>%
#   select(sid, status_eoy_yr4)
# stuOT <- left_join(stuOT, tmp, by = "sid")




# Highly qualified

# cg_student$highly_qualified <- NA
# cg_student$highly_qualified[!is.na(cg_student$chrt_grad) &
#                               cg_student$cum_gpa_final >= 3.7 &
#                               !is.na(cg_student$cum_gpa_final) &
#                               cg_student$sat_act_concordance >= 1100 &
#                               !is.na(cg_student$sat_act_concordance)] <- 1
# cg_student$highly_qualified[!is.na(cg_student$chrt_grad) &
#                               cg_student$cum_gpa_final >= 3.3 &
#                               !is.na(cg_student$cum_gpa_final) &
#                               cg_student$sat_act_concordance >= 1200 &
#                               !is.na(cg_student$sat_act_concordance)] <- 1
# cg_student$highly_qualified[!is.na(cg_student$chrt_grad) &
#                               cg_student$cum_gpa_final >= 3.0 &
#                               !is.na(cg_student$cum_gpa_final) &
#                               cg_student$sat_act_concordance >= 1300 &
#                               !is.na(cg_student$sat_act_concordance)] <- 1
# cg_student$highly_qualified[!is.na(cg_student$chrt_grad) &
#                               is.na(cg_student$highly_qualified)] <- 0
# table(is.na(cg_student$highly_qualified[is.na(cg_student$chrt_grad)]))
#########################
## CEDS
######################

#' Title
#'
#' @param simouts
#' @param output
#'
#' @return
#' @export
#'
#' @examples
ceds_cleaner <- function(simouts, output = "directory", directory = NULL){
  if(output == "directory" & is.null(directory)){
    directory <- tempdir()
  }
  if(!file.exists(directory)){
    dir.create(directory)
  }


  k12_student_identity <- data.frame(
    "Student Identification System" = simouts$demog_master$id_type,
    "Student Identifier" = simouts$demog_master$sid)

  if(!"Native.Hawaiian.or.Other.Pacific.Islander" %in% colnames(simouts$demog_master)){
    simouts$demog_master$Native.Hawaiian.or.Other.Pacific.Islander <- 0
  }

  k12_student_demographic <- data.frame(
    "Student Identifier" = simouts$demog_master$sid,
    "American Indian or Alaska Native" = simouts$demog_master$American.Indian.or.Alaska.Native,
    "Asian" = simouts$demog_master$Asian,
    "Birthdate" = simouts$demog_master$Birthdate,
    "Black or African American" = simouts$demog_master$Black.or.African.American,
    "Demographic Race Two or More Races" = simouts$demog_master$Demographic.Race.Two.or.More.Races,
    "Hispanic or Latino Ethnicity" = simouts$demog_master$Hispanic.or.Latino.Ethnicity,
    "Native Hawaiian or Other Pacific Islander" = simouts$demog_master$Native.Hawaiian.or.Other.Pacific.Islander,
    "Sex" = simouts$demog_master$Sex,
    "White" = simouts$demog_master$White
    )


  k12_limited_english_proficiency <- data.frame(
    "Student Identifier" = simouts$stu_year$sid,
    "Limited English Proficiency Entry Date" = simouts$stu_year$year,
    "Limited English Proficiency Exit Date" = simouts$stu_year$year + 1,
    "Limited English Proficiency Status" = simouts$stu_year$ell,
    "Title III Limited English Proficient Participation Status" = simouts$stu_year$ell
  )
  k12_student_economically_disadvantaged <- data.frame(
    "Student Identifier" = simouts$stu_year$sid,
    "Economic Disadvantage Status" = simouts$stu_year$frpl,
    "Participation in School Food Service Programs" = simouts$stu_year$frpl,
    "Status End Date" = simouts$stu_year$year + 1,
    "Status Start Date" = simouts$stu_year$year
  )

  # TODO Add entry code
  if(!"entry_code" %in% names(simouts$stu_year)){
    simouts$stu_year$entry_code <- "018001"
  }
  if(!"lea_id" %in% names(simouts$stu_year)){
    simouts$stu_year <- ungroup(simouts$stu_year)
    simouts$stu_year$lea_id <- "0001"
  }

  k12_student_enrollment <- data.frame(
    "Student Identifier" = simouts$stu_year$sid,
    "Cohort Year" = simouts$stu_year$cohort_year,
    "Cohort Graduation Year" = simouts$stu_year$cohort_grad_year,
    "Enrollment Entry Date" = simouts$stu_year$year,
    "Enrollment Exit Date" = simouts$stu_year$year +1,
    "Enrollment Status" = simouts$stu_year$enrollment_status,
    "Entry Grade Level" = simouts$stu_year$grade,
    "Entry Type" = simouts$stu_year$entry_code,
    "Exit Grade Level" = simouts$stu_year$grade_advance,
    "Exit or Withdrawal Status" = simouts$stu_year$exit_type,
    "End Of Term Status" = simouts$stu_year$grade_advance,
    "Gifted and Talented Indicator" = simouts$stu_year$gifted,
    "School Identifier" = simouts$stu_year$schid,
    "Local Education Agency Identifier" = simouts$stu_year$lea_id,
    "Resonsible District Identifier" = simouts$stu_year$lea_id,
    "Responsible School Identifier" = simouts$stu_year$schid,
    "Responsible School Type" = "Accountable",
    "Responsible District Type" = "Accountable"
  )
  k12_student_academic_record <- data.frame(
    "Student Identifier" = simouts$hs_annual$sid,
    "Year" = simouts$hs_annual$year,
    "School Identifier" = simouts$hs_annual$schid,
    "Credits Earned Cumulative" = simouts$hs_annual$cum_credits,
    "Credits Attempted Cumulative" = simouts$hs_annual$cum_credits_attempted,
    "Diploma Or Credential Award Date" = simouts$hs_annual$expected_grad_hs,
    "GPA Cumulative" = simouts$hs_annual$cum_gpa,
    "Graduation Rate Survey Indicator" = simouts$hs_annual$grad_cohort_ind,
    "Graduation Rate Survey Cohort Year" = simouts$hs_annual$expected_grad_hs,
    "Number of Credits Attempted" = simouts$hs_annual$credits_attempted,
    "Number of Credits Earned" = simouts$hs_annual$credits_earned,
    "Projected Graduation Date" = simouts$hs_annual$expected_grad_hs
  )

  k12_student_academic_record_tmp <- data.frame(
    "Student Identifier" = simouts$hs_outcomes$sid,
    "High School Diploma Type" = simouts$hs_outcomes$diploma_type,
    "High School Student Class Rank" = simouts$hs_outcomes$class_rank,
    "Postsecondary Enrollment Action" = simouts$hs_outcomes$ps
  )
  k12_student_academic_record <- k12_student_academic_record[!duplicated(k12_student_academic_record),]

  k12_student_academic_record <- left_join(k12_student_academic_record,
                                           k12_student_academic_record_tmp,
                                           by = "Student.Identifier")
  k12_student_academic_record_tmp <- NULL
  k12_student_academic_record <- k12_student_academic_record[!duplicated(k12_student_academic_record),]


  k12_student_attendance <- data.frame(
    "Student Identifier" = simouts$stu_year$sid,
    "Number of Days Absent" = simouts$stu_year$ndays_possible - simouts$stu_year$ndays_attend,
    "Number of Days in Attendance" = simouts$stu_year$ndays_attend,
    "Student Attendance Rate" = simouts$stu_year$att_rate
  )
  k12_school_identification <- data.frame(
    "Name of Institution" = simouts$schools$name,
    "Organization Type" = simouts$schools$type,
    "School Identifier" = simouts$schools$schid,
    "School Identification System" = simouts$schools$id_type,
    "Local Education Agency Identifier" = simouts$schools$lea_id,
    "Short Name of Institution" = simouts$schools$name
  )
  k12_school_institution_characteristics <- data.frame(
    "School Identifier" = simouts$schools$schid,
    "State Poverty Designation" = simouts$schools$poverty_desig,
    "Title I School Status" = simouts$schools$title1_status,
    "Title III Language Instruction Program Type" = simouts$schools$title3_program_type
  )
  ps_institution_directory <- data.frame(
    "Name of Institution" = simouts$nsc$name,
    "Short Name of Institution" = simouts$nsc$short_name,
    "Institution Identifier" = simouts$nsc$opeid,
    "Institution Identifier Type" = "OPEID"
  )


  ps_student_institution_enrollment <- data.frame(
    "Student Identifier" = simouts$ps_enroll$sid,
    "Enrollment Entry Date" = simouts$ps_enroll$year,
    "Enrollment Exit Date" = simouts$ps_enroll$year + 1,
    "Name of Institution" = simouts$ps_enroll$opeid, # change to name
    "Diploma or Credential Award Date" = "NA"
  )
  assessment_registration <- data.frame(
    # add LEA ID
    "Student Identifier" = simouts$assessments$sid,
    "School Identifier" = simouts$assessments$schid,
    "Year" = simouts$assessments$year,
    "Assessment Registration Grade Level to Be Assessed" = simouts$assessments$grade,
    "Grade Level When Assessed" = simouts$assessments$grade,
    "Assessment Accomodation Category" = NA,
    "Retest Indicator" = simouts$assessments$retest_ind
  )
  assessment_result <- data.frame(
    "Student Identifier" = simouts$assessments$sid,
    "Year" = simouts$assessments$year,
    "Assessment Academic Subject" = simouts$assessments$subject,
    "Assessment Score Metric Type" = simouts$assessments$score_type,
    "Assessment Result Score Value" = simouts$assessments$score,
    "Assessment Identifier" = simouts$assessments$assess_id
  )
  assessment <- data.frame(
    "Assessment Identifier" = simouts$assessments$assess_id,
    "Assessment Short Name" = simouts$assessments$assess_name,
    "Assessment Academic Subject" = simouts$assessments$subject,
    "Assessment Level for Which Designed" = simouts$assessments$grade
  )
  assessment <- assessment[!duplicated(assessment),]

  tmp_output <- function(object, directory){
    filename <- file.path(directory, deparse(substitute(object)))
    filename <- paste0(filename, ".csv")
    write.csv(object, filename, row.names = FALSE)
  }


  tmp_output(k12_student_identity, directory = directory)
  tmp_output(k12_student_demographic, directory = directory)
  tmp_output(k12_limited_english_proficiency, directory = directory)
  tmp_output(k12_student_economically_disadvantaged, directory = directory)
  tmp_output(k12_student_enrollment, directory = directory)
  tmp_output(k12_student_academic_record, directory = directory)
  tmp_output(k12_student_attendance, directory = directory)
  tmp_output(k12_school_identification, directory = directory)
  tmp_output(k12_school_institution_characteristics, directory = directory)
  tmp_output(ps_institution_directory, directory = directory)
  tmp_output(k12_student_enrollment, directory = directory)
  tmp_output(ps_student_institution_enrollment, directory = directory)
  tmp_output(assessment_registration, directory = directory)
  tmp_output(assessment_result, directory = directory)
  tmp_output(assessment, directory = directory)
  zip("CEDS_lite.zip", directory)


}

## - Assessment Result Score STandard Error / assessment$error

# assessment performance level
## - Assessment Performance Level Identifier / assessment$prof_level
## - Assessment Performance Level Label / assessment$prof_level_name
## - Assessment Performance Level Lower Cut Score / assessment$min_score
## - Assessment Performance Level Upper Cut Score / assessment$max_score


# transfer_dim <-
# 01821 - Transfer from a public school in the same local education agency
# 01822 - Transfer from a public school in a different local education agency in the same state
# 01823 - Transfer from a public school in a different state
# 01824 - Transfer from a private, non-religiously-affiliated school in the same local education agency
# 01825 - Transfer from a private, non-religiously-affiliated school in a different LEA in the same state
# 01826 - Transfer from a private, non-religiously-affiliated school in a different state
# 01827 - Transfer from a private, religiously-affiliated school in the same local education agency
# 01828 - Transfer from a private, religiously-affiliated school in a different LEA in the same state
# 01829 - Transfer from a private, religiously-affiliated school in a different state
# 01830 - Transfer from a school outside of the country
# 01831 - Transfer from an institution
# 01832 - Transfer from a charter school
# 01833 - Transfer from home schooling
# 01835 - Re-entry from the same school with no interruption of schooling
# 01836 - Re-entry after a voluntary withdrawal
# 01837 - Re-entry after an involuntary withdrawal
# 01838 - Original entry into a United States school
# 01839 - Original entry into a United States school from a foreign country with no interruption in schooling
# 01840 - Original entry into a United States school from a foreign country with an interruption in schooling
# 09999 - Other


# - K12 Student Academic Record (long by term)
## - Diploma or Credential Award Date / hs_outcomes$GRAD_YEAR
## - End of Term Status /  stu_year$PROMOTIONRETENTION
## - GPA Cumulative / hs_annual$cum_gpa
## - Postsecondary Enrollment Action hs_outcomes$ps [Enrolled/NotENrolled/No Information]
## - Projected Graduation Date / hs_annual$expected_grad




# SEA Identification
## Organization Name
## Organization Type
## State Agency Identifier

