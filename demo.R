library(dplyr)
library(wakefield)
library(lubridate)
set.seed(612)
source("R/funcs.R")

demog_master <- r_data_frame(n = 500, 
                             id(random = TRUE), 
                             race, 
                             sex, 
                             # dob, set range of years available for birth
                             dob(start = Sys.Date() - 365 * 25, 
                                 k = 365 * 8, by = "1 days"))

# demog_master$ses <- NA
# demog_master$ses[demog_master$Race == "White"] <- rnorm(10, 0.3, 1)

ses_list <- list("White" = list(f = rnorm, 
                                  pars = list(mean = 0.3, sd = 1.1)), 
                    "Hispanic" = list(f = rnorm, 
                                   pars = list(mean = -0.1, sd = 0.9)),
                    "Black" = list(f = rnorm, 
                                      pars = list(mean = -0.2, sd = 1.2)), 
                    "Asian" = list(f = rnorm, 
                                      pars = list(mean = 0.23, sd = 1.2)), 
                    "Bi-Racial" = list(f = rnorm, 
                                      pars = list(mean = 0.0, sd = 1)), 
                    "Native" = list(f = rnorm, 
                                      pars = list(mean = -0.2, sd = 1)), 
                    "Other" = list(f = rnorm, 
                                      pars = list(mean = 0, sd = 1)),
                    "Hawaiian" = list(f = rnorm, 
                                   pars = list(mean = 0, sd = 1)) 
                    )
# Need SES
demog_master <- as.data.frame(demog_master)
demog_master <- cond_prob(demog_master, factor = "Race", 
                 newvar = "ses", prob_list = ses_list)


# Language should be inferred from race
# language(x = c("English", "Spanish", "Other"), 
#          prob = c(0.75, 0.2, 0.05))

## Generate student-year data

minyear <- 1997
maxyear <- 2016
stu_year <- vector(mode = "list", nrow(demog_master))

for(i in 1:nrow(demog_master)){
  tmp <- expand.grid.df(demog_master[i, c(1, 4)], 
                        data.frame(year = 1:12))
  
  tmp$year <- lubridate::year(tmp$DOB + (tmp$year + 4) * 365)
  tmp$year - lubridate::year(tmp$DOB)
  stu_year[[i]] <- tmp; rm(tmp)
}

stu_year <- bind_rows(stu_year)

stu_year$age <- age_calc(dob = stu_year$DOB, 
                         enddate = as.Date(paste0(stu_year$year, "-09-21")),
                         units = "years", precise = TRUE)



# Create grades

