library(wakefield)
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

demog_master <- r_data_frame(n = 500, 
                             id(random = TRUE), 
                             race, 
                             sex, 
                             # dob, set range of years available for birth
                             dob(start = Sys.Date() - 365 * 25, 
                                 k = 365 * 8, by = "1 days"))

height_list <- list("Male" = list(f = height_in, pars = list(mean = 70, sd = 3.8)), 
                    "Female" = list(f = height_in, pars = list(mean = 64, sd = 3.2)))
demog_master <- as.data.frame(demog_master)
out <- cond_prob(demog_master, factor = "Sex", 
                 newvar = "height", prob_list = height_list)

# Need SES
# Need language
# Need "ability/IQ"

# Language should be inferred from race
# language(x = c("English", "Spanish", "Other"), 
#          prob = c(0.75, 0.2, 0.05))


expand.grid.df(demog_master[1,], data.frame(grade = 1:12))
