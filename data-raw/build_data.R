# Build data
library(purrr)

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
  xwalk$schema[i] <- I(list(get_code_values(xwalk$Option.Set[i])))
}

# saveRDS(xwalk, "data/sdp_ceds_map.rds")
use_data(ell, xwalk, internal = TRUE)
