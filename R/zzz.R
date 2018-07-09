#' @importFrom stats rbinom runif pnorm setNames coef rnbinom vcov simulate update
#' rnorm rpois sd sigma terms time reshape
#' @importFrom stats na.omit
#' @importFrom utils data head tail write.csv zip
#' @importFrom methods new

.onAttach = function(...) {
  if (!interactive()) return()
  msg = "Welcome to OpenSDP." # nocov
  packageStartupMessage(paste(strwrap(msg), collapse = "\n")) # nocov
}


utils::globalVariables(c("Sex", "iep", "Race", "frpl", "age"))


.onLoad <- function(libname = find.package("OpenSDPsynthR"), pkgname = "OpenSDPsynthR"){
    # CRAN Note avoidance
  if(getRversion() >= "2.15.1")
    utils::globalVariables(
      # from simpop and subset function calls
      c("sid", "grad", "first_flag", "schools_race_prob", "White",
        "initschid", "Lodgment_method", "PHI_Ind", "Sw_amt", "Alow_ben_amt",
        "ontrack_yr1", "grade", "math_ss", "rdg_ss",
        "ontrack_yr4", "cum_credits_yr1", "cum_credits_yr4", "cum_credits_yr1_ela",
        "cum_credits_yr4_ela", "cum_credits_yr1_math", "cum_credits_yr4_math",
        "cum_gpa_yr1", "cum_gpa_yr4", "yr_seq",
        "scale_gpa", "grad_prob", "ps_prob", "ps",
        "cum_credits", "credits_earned", "credits_attempted", "grad",
        "hs_status", "status_after", "event",
        "ps_transfer", "opeid", "ps_change_ind", "short_name",
        "type",
        # we use the magrittr pipe
        ".",
        # CLEANER
        "grade", "sid", "schid", "gifted", "chrt_ninth", "name", "keep", "math_ss",
        "rdg_ss", "test_math_8_std", "test_ela_8_std", "test_composite_8",
        "yr_seq", "ontrack", "cum_credits", "cum_credits_ela", "cum_credits_math",
        "cum_gpa", "status_after", "scale_gpa", "gpa", "grad_prob", "grad",
        "hs_status", "ps_prob", "ps", "diploma_type", "class_rank", "opeid",
        "first_ps", "last_ps", "chrt_grad", "variable", "value", "temp",
        "1_enrl_1oct_grad", "2_enrl_1oct_grad", "3_enrl_1oct_grad", "4_enrl_1oct_grad",
        "5_enrl_1oct_grad", "1_enrl_1oct_ninth", "2_enrl_1oct_ninth", "3_enrl_1oct_ninth",
        "4_enrl_1oct_ninth", "5_enrl_1oct_ninth", "enrl_ever_w2_grad_any",
        "enrl_ever_w2_ninth_any", "ps_type", "enrl_1oct_grad", "enrl_1oct_ninth",
        "enrl_ever_w2_ninth", "enrl_ever_w2_grad", "enrl_ever_w2_grad_2yr",
        "enrl_ever_w2_grad_4yr", "enrl_ever_w2_ninth_2yr", "enrl_ever_w2_ninth_4yr",
        "term", "ps_short_name", "enroll_count", "all4", "persist", "chrt",
        "observed",
        # control parameters
        "race_list", "frl_list", "school_list", "gifted_list",
        # more simpop
        "flag", "grade_diff", "cohort_year", "subject", "assess_id", "score",
        "ntests", "schid", "cohort_grad_year"
      )
    )
  invisible()
}
