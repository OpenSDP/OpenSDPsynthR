#' Grand simulation
#'
#' @param n integer, number of students to simulate
#' @param seed integer, random seed to use
#' @param race_groups vector of labels for race groups
#' @param race_prob vector of numerics, same length as \code{race_groups}
#' @param ses_list a probability list
#' @param minyear integer, year to start data
#' @param maxyear integer, year to end data
#'
#' @return
#' @importFrom wakefield r_data_frame
#' @importFrom lubridate year
#' @import dplyr
#' @export
#'
#' @examples
#' out <- popsim_control(20, seed = 213)
popsim_control <- function(n, seed, race_groups=NULL, race_prob=NULL,
                           ses_list=NULL, minyear=1997, maxyear=2017){
  set.seed(seed)

  if(missing(race_groups)){
    race_groups <- xwalk$CEDS_name[xwalk$category == "Demographic"]
    race_groups <- race_groups[race_groups != ""]
  }
  if(missing(race_prob)){
    race_prob <- c(0.637, 0.047, 0.007, 0.122, 0.163, 0.021, 0.0015)
  }

  demog_master <- data.frame(
    id = wakefield::id(n, random = TRUE),
    "Sex" = wakefield::sex(n),
    "DOB" = wakefield::dob(n, start = Sys.Date() - 365*25,
                           k = 365 * 8, by = "1 days"),
    "Race" = wakefield::race(n, x = race_groups, prob = race_prob)
    )
  demog_master$Race <- factor(demog_master$Race)

  demog_master %<>% make_inds("Race")
  demog_master %<>% mutate_at(5:ncol(demog_master),
                              funs(recode(., `0` = "No", `1` = "Yes")))
  if(missing(ses_list)){
    ses_list <- OpenSDP.data:::ses_list
  }
  demog_master <- as.data.frame(demog_master)
  demog_master <- cond_prob(demog_master, factor = "Race",
                              newvar = "ses", prob_list = ses_list)
  ## Generate student-year data
  stu_year <- vector(mode = "list", nrow(demog_master))

  # Make a list of dataframes, one for each student, for each year
  for(i in 1:nrow(demog_master)){
    tmp <- expand_grid_df(demog_master[i, c(1, 3)],
                          data.frame(year = 1:12))

    tmp$year <- lubridate::year(tmp$DOB + (tmp$year + 4) * 365)
    tmp$year - lubridate::year(tmp$DOB)
    stu_year[[i]] <- tmp; rm(tmp)
  }

  stu_year <- bind_rows(stu_year) %>% as.data.frame()
  stu_year$age <- age_calc(dob = stu_year$DOB,
                           enddate = as.Date(paste0(stu_year$year, "-09-21")),
                           units = "years", precise = TRUE)


  return(list(demog_master = demog_master, stu_year = stu_year))
}
