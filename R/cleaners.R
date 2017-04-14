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
      chrt_hs = min(year[grade == "12"]),
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

  # Generate attend/dropout for years 1 - 4
  # Generate stay/transfer for years 1-4
  # Generate OPEID for years 1-4 where still enrolled

  head(simouts$hs_outcomes)



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
  demog_clean <- simouts$demog_master %>%
    summarize(
      sid = sid,
      male = if_else(Sex == "Male", 1, 0),
      race_ethnicity = Race
    )
  final_data <- left_join(demog_clean, hs_summary)
  final_data <- left_join(final_data, outcomes_clean)
  return(final_data)
}
