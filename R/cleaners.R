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

  scores <- simouts$assessment %>% ungroup %>%
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
