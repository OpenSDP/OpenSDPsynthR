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
      first_hs = first(schid),
      last_hs = last(schid),
      chrt_ninth = min(year[grade == "9"]),
      chrt_hs = min(year[grade == "12"]),
      frpl_ever = ifelse(any(frpl == "1"), "1", "0"),
      iep_ever = ifelse(any(iep == "1"), "1", "0"),
      ell_ever = ifelse(any(ell == "1"), "1", "0"),
      gifted_ever = ifelse(any(gifted == "1"), "1", "0"),
      nyears = n()
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
  final_data <- left_join(hs_summary, outcomes_clean)
  return(final_data)
}
