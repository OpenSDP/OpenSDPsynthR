# Build data
library(purrr)
library(tidyr)

## Build example ELL baseline data
ell <- read.csv("data-raw/ellDist.csv")
ell <- na.omit(ell)
ell <-  ell %>% gather(-age, key = "race", value = "prob")
# ell <- ell[-1,]
# saveRDS(ell, "data/ell.rds")
# use_data(ell, internal = TRUE)

## Build example SDP/CEDS crosswalk
xwalk <- read.csv("data-raw/CEDS_SDP_map.csv", stringsAsFactors = FALSE)
xwalk$schema <- NA
for(i in 1:nrow(xwalk)){
  xwalk$schema[i] <- I(list(OpenSDP.data:::get_code_values(xwalk$Option.Set[i])))
}

ses <- data.frame(race = c("black", "asian", "hispanic", "amerind", "white",
                           "other", "multiracial", "hawaiian_pi"),
                  prob = c(0.65, 0.375, 0.6, 0.4, 0.4, 0.4, 0.4, 0.4))


# saveRDS(xwalk, "data/sdp_ceds_map.rds")
devtools::use_data(ell, xwalk, ses, internal = TRUE, overwrite = TRUE)
