# Build data
library(purrr)
library(tidyr)

## Build example ELL baseline data
ell <- read.csv("data-raw/ellDist.csv")
ell <- na.omit(ell)
ell %<>% gather(-age, key = "race", value = "prob")
# ell <- ell[-1,]
# saveRDS(ell, "data/ell.rds")
# use_data(ell, internal = TRUE)

## Build example SDP/CEDS crosswalk
xwalk <- read.csv("data-raw/CEDS_SDP_map.csv", stringsAsFactors = FALSE)
xwalk$schema <- NA
for(i in 1:nrow(xwalk)){
  xwalk$schema[i] <- I(list(OpenSDP.data:::get_code_values(xwalk$Option.Set[i])))
}

ses_list <- list("White" = list(f = rbinom,
                            pars = list(size = 1, prob = 0.4)),
                   "Hispanic or Latino Ethnicity" = list(f = rbinom,
                                           pars = list(size = 1, prob = 0.6)),
                   "Black or African American" = list(f = rbinom,
                                    pars = list(size = 1, prob = 0.65)),
                   "Asian" = list(f = rbinom,
                                  pars = list(size = 1, prob = 0.375)),
                   "Demographic Race Two or More Races" = list(f = rbinom,
                                           pars = list(size = 1, prob = 0.4)),
                   "American Indian or Alaska Native" = list(f = rbinom,
                                           pars = list(size = 1, prob = 0.4)),
                   "Other" = list(f = rbinom,
                                  pars = list(size = 1, prob = 0.4)),
                   "Native Hawaiian or Other Pacific Islander" = list(f = rbinom,
                                          pars = list(size = 1, prob = 0.4)))


# saveRDS(xwalk, "data/sdp_ceds_map.rds")
devtools::use_data(ell, xwalk, ses_list, internal = TRUE, overwrite = TRUE)
