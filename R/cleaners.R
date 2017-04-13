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

  head(simouts$hs_outcomes)

  ps_pool <- simouts$hs_outcomes[simouts$hs_outcomes$ps == 1,
                                 c("sid", "ps_prob", "grad", "gpa", "ps")]

  # Generate attend/dropout for years 1 - 4
  # Generate stay/transfer for years 1-4
  # Generate OPEID for years 1-4 where still enrolled

  big <- crossing(sid = as.character(unique(ps_pool$sid)), year = 1:4)
  ps_pool <- left_join(big, ps_pool, by = "sid")

  ps_pool <- ps_pool %>% group_by(sid) %>% arrange(sid, year)
  ps_pool$ps[ps_pool$year > 1] <- sapply(ps_pool$ps_prob[ps_pool$year > 1],
                                             function(x) rbinom(1, 1, prob = x))

  tm <- matrix(
    c(100, 700, 90, 900),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(c("1", "0"), c("1", "0"))
  )
  ps_transfer_list <- list("ALL" =
                             list(f = make_markov_series,
                                  pars = list(tm = tm_convert(tm))))
  ps_pool <- ps_pool %>% group_by(sid) %>% arrange(sid, year) %>%
    mutate(ps_transfer = markov_cond_list("ALL", n = n()-1, ps_transfer_list,
                                          t0 = sample(c("0", "1"), 1, prob = c(0.8, 0.2)),
                                          include.t0 = TRUE)
             )
  ps_pool <- ps_pool %>% group_by(sid) %>% arrange(sid, year) %>%
    mutate(opeid = sample(simouts$nsc$opeid, 1, prob = simouts$nsc$enroll))

  opeid_changer <- function(opeid){
    sample(simouts$nsc$opeid[simouts$nsc$opeid != opeid],
           1,
           prob =simouts$nsc$enroll[simouts$nsc$opeid != opeid])
  }

  ps_pool <- ps_pool %>% group_by(sid) %>% arrange(sid, year) %>%
    mutate(ps_change_ind = cumsum(ifelse(ps_transfer == "1", 1, 0)))

  # Need to figure out a way to assign students to a new opeid and stick with
  # it until the next transfer
  ps_pool <- ps_pool %>% rowwise() %>%
    mutate(opeid = ifelse(ps_transfer == "1",
                           opeid_changer(opeid),opeid)
             )



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
