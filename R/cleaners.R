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
      longest_hs_code = names(which.max(table(schid))),
      frpl_ever_hs = ifelse(any(frpl == "1"), "1", "0"),
      iep_ever_hs = ifelse(any(iep == "1"), "1", "0"),
      ell_ever_hs = ifelse(any(ell == "1"), "1", "0"),
      gifted_ever_hs = ifelse(any(gifted == "1"), "1", "0"),
      chrt_ninth = min(year[grade == "9"]),
      nyears = n()
    )
  hs_summary <- filter(hs_summary, is.finite(chrt_ninth))
  schools <- as.data.frame(simouts$schools)
  attributes(schools$schid) <- NULL
  hs_summary <- inner_join(hs_summary, schools[, c("schid", "name")],
                           by=setNames("schid", "first_hs_code"))
  hs_summary %<>% rename(first_hs_name = name)
  hs_summary <- left_join(hs_summary, schools[, c("schid", "name")],
                          by=setNames("schid", "last_hs_code"))
  hs_summary %<>% rename(last_hs_name = name)
  hs_summary <- left_join(hs_summary, schools[, c("schid", "name")],
                          by=setNames("schid", "longest_hs_code"))
  hs_summary %<>% rename(longest_hs_name = name)





  demog_clean <- simouts$demog_master %>%
    group_by(sid) %>%
    summarize(
      male = if_else(Sex == "Male", 1, 0),
      race_ethnicity = Race[[1]]
    )

  demog_summary <- simouts$stu_year %>%
    group_by(sid) %>% arrange(sid, year) %>%
    summarize(
      frpl_ever = ifelse(any(frpl == "1"), "1", "0"),
      iep_ever = ifelse(any(iep == "1"), "1", "0"),
      ell_ever = ifelse(any(ell == "1"), "1", "0"),
      gifted_ever = ifelse(any(gifted == "1"), "1", "0")
    )
  demog_clean <- left_join(demog_clean, demog_summary)
  rm(demog_summary)

  # Take the most recent score
  scores <- simouts$stu_assess %>% ungroup %>%
    group_by(sid, grade) %>%
    mutate(keep = ifelse(year == min(year), 1, 0)) %>%
    filter(keep == 1) %>% select(-keep) %>%
    ungroup %>%
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

  credits_long <- na.omit(na.omit(as.data.frame(simouts$hs_annual[, c(1, 7, 9:13)])))
  credits_long <- simouts$hs_annual %>%
    select(sid, yr_seq, ontrack, cum_credits, cum_credits_ela, cum_credits_math,
           cum_gpa)
  credits_long <- na.omit(as.data.frame(credits_long))
  credits_wide <- reshape(credits_long,
                          timevar = "yr_seq",
                          sep = "_yr",
                          idvar = "sid",
                          direction = "wide")
  rm(credits_long)

  status_vars <- as.data.frame(simouts$hs_annual) %>% select(sid, yr_seq, status_after) %>%
    reshape(timevar = "yr_seq", sep = "_yr", idvar = "sid", direction = "wide")

  # simouts$hs_outcomes$grad_cohort == cohort_grad


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
  outcomes_clean <- outcomes_clean %>% select(-diploma_type, -class_rank)

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

  # do the enrolled thing by yr_seq
  # ensure first that all yr_seq combinations are present
  ps_wide <- reshape(ps_long[, c("sid", "year", "enroll_any", "enroll_full")],
                     timevar = "year",
                     sep = "_yr",
                     idvar = "sid",
                     direction = "wide")
  chrt_data <- simouts$hs_outcomes %>% select(sid, chrt_grad)
  chrt_data <- left_join(chrt_data,
                         simouts$stu_year %>% group_by(sid) %>%
                           summarize(chrt_ninth = min(year[grade == "9"])))
  chrt_data <- chrt_data %>% filter(is.finite(chrt_grad))
  chrt_data$chrt_ninth <- chrt_data$chrt_ninth + 3

  simouts$ps_enroll <- left_join(simouts$ps_enroll, chrt_data)
  # now need to break this out by type, then split wide by year
  ps_wide <- simouts$ps_enroll %>% group_by(sid, yr_seq) %>%
    summarize(enrl_1oct_grad = ifelse(any((min(year[ps == 1]) - chrt_grad) == yr_seq),
                                   1, 0),
           enrl_1oct_ninth = ifelse(any((min(year[ps == 1]) - chrt_ninth) == yr_seq),
                                   1, 0),
           enrl_ever_w2_grad = ifelse(any((min(year[ps == 1]) - chrt_grad) <= yr_seq + 1),
                                 1, 0),
           enrl_ever_w2_ninth = ifelse(any((min(year[ps == 1]) - chrt_ninth) <= yr_seq + 1),
                                      1, 0)) %>%
    tidyr::gather(variable, value, -(sid:yr_seq)) %>%
    tidyr::unite(temp, yr_seq, variable) %>%
    tidyr::spread(temp, value, fill = 0) %>%
    rename(
      enrl_1oct_grad_yr1_any = `1_enrl_1oct_grad`,
      enrl_1oct_grad_yr2_any = `2_enrl_1oct_grad`,
      enrl_1oct_grad_yr3_any = `3_enrl_1oct_grad`,
      enrl_1oct_grad_yr4_any = `4_enrl_1oct_grad`,
      enrl_1oct_grad_yr5_any = `5_enrl_1oct_grad`,
      enrl_1oct_ninth_yr1_any = `1_enrl_1oct_ninth`,
      enrl_1oct_ninth_yr2_any = `2_enrl_1oct_ninth`,
      enrl_1oct_ninth_yr3_any = `3_enrl_1oct_ninth`,
      enrl_1oct_ninth_yr4_any = `4_enrl_1oct_ninth`,
      enrl_1oct_ninth_yr5_any = `5_enrl_1oct_ninth`
    )
  # enrl_ever_w2_grad_any

  tmp <- ps_wide %>% select(sid, contains("enrl_ever"))
  tmp$enrl_ever_w2_grad_any <- tmp$`1_enrl_ever_w2_grad` + tmp$`2_enrl_ever_w2_grad` +
    tmp$`3_enrl_ever_w2_grad` + tmp$`4_enrl_ever_w2_grad`
  tmp$enrl_ever_w2_ninth_any <- tmp$`1_enrl_ever_w2_ninth` + tmp$`2_enrl_ever_w2_ninth` +
    tmp$`3_enrl_ever_w2_ninth` + tmp$`4_enrl_ever_w2_ninth`

  tmp %<>% select(sid, enrl_ever_w2_grad_any, enrl_ever_w2_ninth_any)
  tmp$enrl_ever_w2_grad_any <- ifelse(tmp$enrl_ever_w2_grad_any > 0, 1, 0)
  tmp$enrl_ever_w2_ninth_any <- ifelse(tmp$enrl_ever_w2_ninth_any > 0, 1, 0)
  ps_wide %<>% select(sid, contains("1oct_"))
  ps_wide <- left_join(ps_wide, tmp); rm(tmp)

  simouts$ps_enroll$ps_type[is.na(simouts$ps_enroll$ps_type)] <- "other"
  simouts$ps_enroll$ps_type <- as.factor(simouts$ps_enroll$ps_type)

  zzz <- simouts$ps_enroll %>% group_by(sid, yr_seq, ps_type) %>%
    tidyr::complete(sid, yr_seq, ps_type) %>%
    group_by(sid, yr_seq, ps_type) %>%
    summarize(enrl_1oct_grad = ifelse(any((min(year[ps == 1]) - chrt_grad) == yr_seq),
                                      1, 0),
              enrl_1oct_ninth = ifelse(any((min(year[ps == 1]) - chrt_ninth) == yr_seq),
                                       1, 0),
              enrl_ever_w2_ninth = ifelse(any((min(year[ps == 1]) - chrt_ninth) <= (yr_seq + 1)),
                                     1, 0),
              enrl_ever_w2_grad = ifelse(any((min(year[ps == 1]) - chrt_grad) <= (yr_seq + 1)),
                                     1, 0)) %>%
    mutate(enrl_1oct_grad = zeroNA(enrl_1oct_grad),
           enrl_1oct_ninth = zeroNA(enrl_1oct_ninth),
           enrl_ever_w2_ninth = zeroNA(enrl_ever_w2_ninth),
           enrl_ever_w2_grad = zeroNA(enrl_ever_w2_grad)) %>%
    tidyr::gather(variable, value, -(sid:ps_type)) %>%
    tidyr::unite(temp, ps_type, yr_seq, variable) %>%
    tidyr::spread(temp, value, fill = 0)

  tmp <- zzz %>% select(sid, contains("enrl_ever_w2"))
  tmp$enrl_ever_w2_grad_2yr <- tmp$`2yr_1_enrl_ever_w2_grad` + tmp$`2yr_2_enrl_ever_w2_grad` +
    tmp$`2yr_3_enrl_ever_w2_grad` + tmp$`2yr_4_enrl_ever_w2_grad`
  tmp$enrl_ever_w2_grad_4yr <- tmp$`4yr_1_enrl_ever_w2_grad` + tmp$`4yr_2_enrl_ever_w2_grad` +
    tmp$`4yr_3_enrl_ever_w2_grad` + tmp$`4yr_4_enrl_ever_w2_grad`
  tmp$enrl_ever_w2_ninth_2yr <- tmp$`2yr_1_enrl_ever_w2_ninth` + tmp$`2yr_2_enrl_ever_w2_ninth` +
    tmp$`2yr_3_enrl_ever_w2_ninth` + tmp$`2yr_4_enrl_ever_w2_ninth`
  tmp$enrl_ever_w2_ninth_4yr <- tmp$`4yr_1_enrl_ever_w2_ninth` + tmp$`4yr_2_enrl_ever_w2_ninth` +
    tmp$`4yr_3_enrl_ever_w2_ninth` + tmp$`4yr_4_enrl_ever_w2_ninth`

  tmp <- tmp %>% select(sid, enrl_ever_w2_grad_2yr, enrl_ever_w2_grad_4yr,
                        enrl_ever_w2_ninth_2yr, enrl_ever_w2_ninth_4yr)
  tmp[, 2:5] <- apply(tmp[, 2:5], 2, function(x) ifelse(x > 0, 1, 0))
  zzz <- zzz %>% select(sid, contains("enrl_1oct"))
  zzz <- left_join(zzz, tmp)
  ps_wide <- left_join(ps_wide, zzz, by = "sid")
  rm(zzz, tmp)

  simouts$ps_enroll$ps_type %<>% as.character()
  sdp_ps <- simouts$ps_enroll %>%  group_by(sid) %>%
    arrange(sid, year, desc(term)) %>%
    select(sid, opeid, ps_short_name, ps_type) %>%
    summarize(first_college_opeid_any = opeid[1],
              first_college_opeid_2yr = opeid[ps_type == "2yr"][1],
              first_college_opeid_4yr = opeid[ps_type == "4yr"][1],
              first_college_name_any = ps_short_name[1], # make selection of first element safe
              first_college_name_2yr = ps_short_name[ps_type == "2yr"][1],
              first_college_name_4yr = ps_short_name[ps_type == "4yr"][1]
              )
  simouts$ps_enroll$ps_type %<>% as.factor()

  ps_career <- simouts$ps_enroll %>% group_by(sid, ps_type) %>%
    tidyr::complete(sid, ps_type) %>%
    summarise(enroll_count = sum(ps),
            chrt_grad = ifelse(any((min(year[ps == 1]) - chrt_grad) == 1),
                   1, 0),
            chrt_ninth = ifelse(any((min(year[ps == 1]) - chrt_ninth) == 1),
                               1, 0)) %>%
    mutate(all4 = ifelse(enroll_count >= 4, 1, 0)
           ) %>%
    mutate(enroll_count = zeroNA(enroll_count),
           chrt_grad = zeroNA(chrt_grad),
           chrt_ninth = zeroNA(chrt_ninth),
           all4 = zeroNA(all4)
           )

  ps_career2 <- simouts$ps_enroll %>%
    group_by(sid) %>%
    mutate(persist = ifelse(any(ps[yr_seq == 2] == 1), 1, 0)) %>%
    group_by(sid, ps_type) %>%
    tidyr::complete(sid, ps_type) %>%
    summarise(persist = ifelse(any(persist > 0), 1, 0),
              chrt_grad = ifelse(any((min(year[ps == 1]) - chrt_grad) == 1),
                                 1, 0),
              chrt_ninth = ifelse(any((min(year[ps == 1]) - chrt_ninth) == 1),
                                  1, 0)) %>%
    mutate(persist = zeroNA(persist),
           chrt_grad = zeroNA(chrt_grad),
           chrt_ninth = zeroNA(chrt_ninth)
    )

  ps_career <- left_join(ps_career, ps_career2)
  ps_career$enroll_count <- NULL
  ps_career %<>%
    tidyr::gather(key = "chrt", value = "observed", chrt_grad, chrt_ninth)
 # compute any
  ps_career <- bind_rows(ps_career,
                         ps_career %>% group_by(sid, chrt) %>%
                           summarize(ps_type = "any",
                                     all4 = max(all4),
                                     persist = max(persist),
                                     observed = max(observed)))

  ps_career %<>% filter(observed == 1) %>%
    filter(!is.na(ps_type)) %>%
    filter(ps_type != "other") %>%
    select(-observed)
  ps_career %<>% gather(key = "persist", value = "obs", all4, persist)
  ps_career <- ps_career %>% gather(variable, value, -(sid:persist)) %>%
    tidyr::unite(temp, chrt, ps_type, persist, variable) %>%
    tidyr::spread(temp, value, fill = 0)
  # ps_career[, 2:ncol(ps_career)] <- apply(ps_career[, 2:ncol(ps_career)], 2, zeroNA)

  sdp_ps <- left_join(sdp_ps, ps_wide, by = "sid")
  sdp_ps <- left_join(sdp_ps, ps_career, by = "sid")

  final_data <- left_join(demog_clean, hs_summary, by = "sid")
  final_data <- left_join(final_data, status_vars, by = "sid")
  final_data <- left_join(final_data, outcomes_wide, by = "sid")
  final_data <- left_join(final_data, outcomes_clean, by = "sid")
  final_data <- left_join(final_data, sdp_ps, by = "sid")
  final_data <- left_join(final_data, scores, by = "sid")
  final_data <- left_join(final_data, credits_wide, by = "sid")
  final_data$test_math_8 <- final_data$test_math_8_std
  final_data$test_ela_8 <- final_data$test_ela_8_std
  final_data$chrt_grad.y <- NULL
  rename_map <- c("late" = "late_grad",
                  "still_enroll" = "still_enrl",
                  "ontime" = "ontime_grad",
                  "ontrack_yr1" = "ontrack_endyr1",
                  "ontrack_yr2" = "ontrack_endyr2",
                  "ontrack_yr3" = "ontrack_endyr3",
                  "ontrack_yr4" = "ontrack_endyr4",
                  "cum_credits_ela_yr1" = "cum_credits_yr1_ela",
                  "cum_credits_math_yr1" = "cum_credits_yr1_math",
                  "cum_credits_ela_yr2" = "cum_credits_yr2_ela",
                  "cum_credits_math_yr2" = "cum_credits_yr2_math",
                  "cum_credits_ela_yr3" = "cum_credits_yr3_ela",
                  "cum_credits_math_yr3" = "cum_credits_yr3_math",
                  "cum_credits_ela_yr4" = "cum_credits_yr4_ela",
                  "cum_credits_math_yr4" = "cum_credits_yr4_math",
                  "gpa" = "cum_gpa_final",
                  "diploma_type" = "hs_dipoma_type",
                  "grad" = "hs_diploma",
                  "chrt_grad.x" = "chrt_grad",
                  "2yr_1_enrl_1oct_grad" = "enrl_1oct_grad_yr1_2yr",
                  "2yr_1_enrl_1oct_ninth" = "enrl_1oct_ninth_yr1_2yr",
                  "2yr_2_enrl_1oct_grad" = "enrl_1oct_grad_yr2_2yr",
                  "2yr_2_enrl_1oct_ninth" = "enrl_1oct_ninth_yr2_2yr",
                  "2yr_3_enrl_1oct_grad" = "enrl_1oct_grad_yr3_2yr",
                  "2yr_3_enrl_1oct_ninth" = "enrl_1oct_ninth_yr3_2yr",
                  "2yr_4_enrl_1oct_grad" = "enrl_1oct_grad_yr4_2yr",
                  "2yr_4_enrl_1oct_ninth" = "enrl_1oct_ninth_yr4_2yr",
                  "2yr_5_enrl_1oct_grad" = "enrl_1oct_grad_yr5_2yr",
                  "2yr_5_enrl_1oct_ninth" = "enrl_1oct_ninth_yr5_2yr",
                  "4yr_1_enrl_1oct_grad" = "enrl_1oct_grad_yr1_4yr",
                  "4yr_1_enrl_1oct_ninth" = "enrl_1oct_ninth_yr1_4yr",
                  "4yr_2_enrl_1oct_grad" = "enrl_1oct_grad_yr2_4yr",
                  "4yr_2_enrl_1oct_ninth" = "enrl_1oct_ninth_yr2_4yr",
                  "4yr_3_enrl_1oct_grad" = "enrl_1oct_grad_yr3_4yr",
                  "4yr_3_enrl_1oct_ninth" = "enrl_1oct_ninth_yr3_4yr",
                  "4yr_4_enrl_1oct_grad" = "enrl_1oct_grad_yr4_4yr",
                  "4yr_4_enrl_1oct_ninth" = "enrl_1oct_ninth_yr4_4yr",
                  "4yr_5_enrl_1oct_grad" = "enrl_1oct_grad_yr5_4yr",
                  "4yr_5_enrl_1oct_ninth" = "enrl_1oct_ninth_yr5_4yr",
                  "other_1_enrl_1oct_grad" = "enrl_1oct_grad_yr1_other",
                  "other_1_enrl_1oct_ninth" = "enrl_1oct_ninth_yr1_other",
                  "other_2_enrl_1oct_grad" = "enrl_1oct_grad_yr2_other",
                  "other_2_enrl_1oct_ninth" = "enrl_1oct_ninth_yr2_other",
                  "other_3_enrl_1oct_grad" = "enrl_1oct_grad_yr3_other",
                  "other_3_enrl_1oct_ninth" = "enrl_1oct_ninth_yr3_other",
                  "other_4_enrl_1oct_grad" = "enrl_1oct_grad_yr4_other",
                  "other_4_enrl_1oct_ninth" = "enrl_1oct_ninth_yr4_other",
                  "other_5_enrl_1oct_grad" = "enrl_1oct_grad_yr5_other",
                  "other_5_enrl_1oct_ninth" = "enrl_1oct_ninth_yr5_other",
                  "chrt_grad_4yr_persist_obs" = "enrl_grad_persist_4yr",
                  "chrt_grad_2yr_persist_obs" = "enrl_grad_persist_2yr",
                  "chrt_grad_any_persist_obs" = "enrl_grad_persist_any",
                  "chrt_ninth_4yr_persist_obs" = "enrl_ninth_persist_4yr",
                  "chrt_ninth_2yr_persist_obs" = "enrl_ninth_persist_2yr",
                  "chrt_ninth_any_persist_obs" = "enrl_ninth_persist_any",
                  "chrt_grad_4yr_all4_obs" = "enrl_grad_all4_4yr",
                  "chrt_grad_2yr_all4_obs" = "enrl_grad_all4_2yr",
                  "chrt_grad_any_all4_obs" = "enrl_grad_all4_any",
                  "chrt_ninth_4yr_all4_obs" = "enrl_ninth_all4_4yr",
                  "chrt_ninth_2yr_all4_obs" = "enrl_ninth_all4_2yr",
                  "chrt_ninth_any_all4_obs" = "enrl_ninth_all4_any")

  names(final_data)[names(final_data) %in% names(rename_map)] %<>% rename_map[.]

  final_data$last_wd_group <- NA
  final_data$last_wd_group[final_data$hs_diploma == 1 &
                             !is.na(final_data$hs_diploma)] <- "Graduated"
  final_data$last_wd_group[final_data$hs_diploma == 0 &
                             !is.na(final_data$hs_diploma)] <- final_data$hs_status[final_data$hs_diploma == 0 &
                                                                                    !is.na(final_data$hs_diploma)]
  final_data$last_wd_group[final_data$last_wd_group == "transferout"&
                             !is.na(final_data$hs_diploma)] <- "Transfer Out"
  final_data$last_wd_group[final_data$last_wd_group == "dropout"&
                             !is.na(final_data$hs_diploma)] <- "Drop Out"
  final_data$last_wd_group[final_data$last_wd_group %in% c("disappear", "still_enroll", "dropout") &
                             !is.na(final_data$hs_diploma)] <- "Other"
  final_data$chrt_grad[!is.finite(final_data$chrt_grad)] <- NA
  final_data$hs_diploma_date <- paste0("05/25/", final_data$chrt_grad)
  final_data$hs_diploma_date[is.na(final_data$chrt_grad)] <- NA
  final_data$highly_qualified <- sapply(final_data$ps_prob - 0.2, rbinom, n=1, size=1)
  final_data$highly_qualified[is.na(final_data$highly_qualified)] <- 0
  final_data$status_after_yr1 <- ifelse(final_data$status_after_yr1 == "still_enroll" &
                                          final_data$ontrack_endyr1 == 1,
                                        "Enrolled, On-Track",
                                        ifelse(final_data$status_after_yr1 == "still_enroll" &
                                                 final_data$ontrack_endyr1 == 0,
                                               "Enrolled, Off-Track", final_data$status_after_yr1))
  final_data$status_after_yr2 <- ifelse(final_data$status_after_yr2 == "still_enroll" &
                                          final_data$ontrack_endyr2 == 1,
                                        "Enrolled, On-Track",
                                        ifelse(final_data$status_after_yr2 == "still_enroll" &
                                                 final_data$ontrack_endyr2 == 0,
                                               "Enrolled, Off-Track", final_data$status_after_yr2))
  final_data$status_after_yr3 <- ifelse(final_data$status_after_yr3 == "still_enroll" &
                                          final_data$ontrack_endyr3 == 1,
                                        "Enrolled, On-Track",
                                        ifelse(final_data$status_after_yr3 == "still_enroll" &
                                                 final_data$ontrack_endyr3 == 0,
                                               "Enrolled, Off-Track", final_data$status_after_yr3))
  final_data$status_after_yr4 <- ifelse(final_data$status_after_yr4 == "still_enroll" &
                                          final_data$ontrack_endyr4 == 1,
                                        "Enrolled, On-Track",
                                        ifelse(final_data$status_after_yr4 == "still_enroll" &
                                                 final_data$ontrack_endyr4 == 0,
                                           "Enrolled, Off-Track", final_data$status_after_yr4))
  final_data$status_after_yr3[final_data$hs_status == "early"] <- "Graduated On-Time"
  final_data$status_after_yr4[final_data$hs_status == "early"] <- "Graduated On-Time"
  final_data$status_after_yr4[final_data$hs_status == "ontime"] <- "Graduated On-Time"
  final_data$status_after_yr4[final_data$hs_status == "dropout"] <- "Dropped Out"
  final_data$status_after_yr4[final_data$hs_status == "transferout"] <- "Disappeared"
  final_data$status_after_yr4[final_data$hs_status == "disappear"] <- "Disappeared"
  final_data$status_after_yr4[final_data$hs_status == "late"] <- "Still Enrolled"
  final_data$status_after_yr4[final_data$status_after_yr4 %in%
                                c("Enrolled, On-Track", "Enrolled, Off-Track")] <- "Still Enrolled"
  # Modify for chart
  final_data$status_after_yr4[final_data$status_after_yr4 == "Still Enrolled"] <- "Enrolled, Not Graduated"
  # Recodes
  final_data$race_ethnicity <- as.character(final_data$race_ethnicity)
  final_data$race_ethnicity[final_data$race_ethnicity == "Black or African American"] <- "Black"
  final_data$race_ethnicity[final_data$race_ethnicity == "Asian"] <- "Asian"
  final_data$race_ethnicity[final_data$race_ethnicity == "Hispanic or Latino Ethnicity"] <- "Hispanic"
  final_data$race_ethnicity[final_data$race_ethnicity == "American Indian or Alaska Native"] <- "Native American"
  final_data$race_ethnicity[final_data$race_ethnicity == "White"] <- "White"
  final_data$race_ethnicity[final_data$race_ethnicity %in% c("Demographic Race Two or More Races",
                                                             "Native Hawaiian or Other Pacific Islander")] <- "Multiple/Other"

  final_data$race_ethnicity <- factor(final_data$race_ethnicity,
                                      levels = c("Black", "Asian", "Hispanic",
                                                 "Native American", "White", "Multiple/Other"))
  final_data$ontrack_sample <- 1
  final_data$ontrack_sample <- ifelse(is.na(final_data$ontrack_endyr1), 0, final_data$ontrack_sample)
  # Type conversion
  message("Converting data types")
  final_data$sid <- as.numeric(final_data$sid)
  final_data$male <- as.numeric(final_data$male)
  # final_data$race_ethnicity <- labelled(final_data$race_ethnicity)
  final_data$hs_diploma <- as.numeric(final_data$hs_diploma)
  final_data$frpl_ever <- as.numeric(final_data$frpl_ever)
  final_data$frpl_ever_hs <- as.numeric(final_data$frpl_ever_hs)
  final_data$iep_ever <- as.numeric(final_data$iep_ever)
  final_data$iep_ever_hs <- as.numeric(final_data$iep_ever_hs)
  final_data$ell_ever <- as.numeric(final_data$ell_ever)
  final_data$ell_ever_hs <- as.numeric(final_data$ell_ever_hs)
  final_data$gifted_ever <- as.numeric(final_data$gifted_ever)
  final_data$first_hs_code <- as.numeric(final_data$first_hs_code)
  final_data$last_hs_code <- as.numeric(final_data$last_hs_code)
  final_data$longest_hs_code <- as.numeric(final_data$longest_hs_code)
  final_data$last_wd_group <- factor(final_data$last_wd_group,
                                        levels = c("Graduated", "Transfer Out",
                                                   "Drop Out", "Other"))
  final_data$ontime_grad <- as.numeric(final_data$ontime_grad)
  final_data$late_grad <- as.numeric(final_data$late_grad)
  final_data$still_enrl <- as.numeric(final_data$still_enrl)
  final_data$transferout <- as.numeric(final_data$transferout)
  final_data$dropout <- as.numeric(final_data$dropout)
  final_data$disappear <- as.numeric(final_data$disappear)
  enrlVec <- names(final_data)[grepl("enrl", names(final_data))]
  # Stata does not want missing data
  final_data[, enrlVec] <- apply(final_data[, enrlVec], 2, as.numeric)
  ontrackVec <- names(final_data)[grepl("ontrack", names(final_data))]
  final_data[, ontrackVec] <- apply(final_data[, ontrackVec], 2, as.numeric)
  final_data$status_after_yr1[final_data$status_after_yr1 == "dropout"] <- "Dropped Out"
  final_data$status_after_yr1[final_data$status_after_yr1 == "transferout"] <- "Disappeared"
  final_data$status_after_yr1[final_data$status_after_yr1 == "disappear"] <- "Disappeared"
  final_data$status_after_yr2[final_data$status_after_yr2 == "dropout"] <- "Dropped Out"
  final_data$status_after_yr2[final_data$status_after_yr2 == "transferout"] <- "Disappeared"
  final_data$status_after_yr2[final_data$status_after_yr2 == "disappear"] <- "Disappeared"
  final_data$status_after_yr3[final_data$status_after_yr3 == "dropout"] <- "Dropped Out"
  final_data$status_after_yr3[final_data$status_after_yr3 == "transferout"] <- "Disappeared"
  final_data$status_after_yr3[final_data$status_after_yr3 == "disappear"] <- "Disappeared"
  final_data$status_after_yr1 <- factor(final_data$status_after_yr1,
                                        levels = c("Enrolled, On-Track",
                                                   "Enrolled, Off-Track",
                                                   "Dropped Out",
                                                   "Disappeared"))
  final_data$status_after_yr2 <- factor(final_data$status_after_yr2,
                                        levels = c("Enrolled, On-Track",
                                                   "Enrolled, Off-Track",
                                                   "Dropped Out",
                                                   "Disappeared"))
  final_data$status_after_yr3 <- factor(final_data$status_after_yr3,
                                        levels = c("Enrolled, On-Track",
                                                   "Enrolled, Off-Track",
                                                   "Dropped Out",
                                                   "Disappeared"))
  final_data$status_after_yr4 <- factor(final_data$status_after_yr4,
                                        levels = c("Graduated On-Time",
                                                   "Enrolled, Not Graduated",
                                                   "Dropped Out",
                                                   "Disappeared"))
  inSamp <- simouts$stu_year %>% filter(grade == "12") %>%
    select(sid) %>% ungroup %>% mutate(sid = as.numeric(sid)) %>% unlist %>% unique()
  # G12 Cohort students who made it to grade 12 only
  final_data$ontrack_hsgrad_sample <- NA
  final_data$ontrack_hsgrad_sample[final_data$sid %in% inSamp] <- 1
  final_data$ontrack_hsgrad_sample <- as.numeric(final_data$ontrack_hsgrad_sample)
  # selectivity
  # // Step 2: Link the analysis file with the college selectivity table to obtain
  # the selectivity level for each college. Use this selectivity information to
  # create college enrollment indicator variables for each college selectivity
  # level. This script assumes that there are 5 levels of selectivity, as in
  # Barron’s College Rankings—Most Competitive (1), Highly Competitive (2),
  # Very Competitive (3), Competitive (4), Least Competitive (5)—as well as a
  # category for colleges without assigned selectivity (assumed to be not
  # competitive).

  # Merge on to subset from above
  final_data <- left_join(final_data, simouts$nsc[, c("opeid", "rank")],
                      by = c("first_college_opeid_4yr" = "opeid"))
  final_data$rank <- factor(final_data$rank, levels = c(1, 2, 3, 4, 5, 6),
               labels = c("Most Competitive", "Highly Competitive",
                          "Very Competitive", "Competitive", "Least Competitive",
                          "Other"))
  return(final_data)
}



#########################
## CEDS
######################

#' CEDS oputput function
#'
#' @param simouts a simulation list resulting from a call to the \code{\link{simpop}}
#' function
#' @param output a character, default "directory", specifying where the output should good
#' @param directory a path to a directory to store the output files
#'
#' @return Nothing. Output is saved out to a file on disk.
#' @export
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

