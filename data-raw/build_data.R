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
  xwalk$schema[i] <- I(list(OpenSDP.data:::get_code_values(xwalk$CEDS_Option_set[i])))
}

ses <- data.frame(race = c("black", "asian", "hispanic", "amerind", "white",
                           "other", "multiracial", "hawaiian_pi"),
                  prob = c(0.65, 0.375, 0.6, 0.4, 0.4, 0.4, 0.4, 0.4))
ses$race <- as.character(ses$race)

prog_baseline <- read.csv("data-raw/program_baseline.csv")
names(prog_baseline) <- tolower(names(prog_baseline))
names(prog_baseline)[1:3] <- map_CEDS(names(prog_baseline)[1:3])
# Next map values
prog_baseline

varName <- "Economic Disadvantage Status"
#
OpenSDP.data:::xwalk$schema[OpenSDP.data:::xwalk$CEDS_name == varName]
get_code_values(OpenSDP.data:::xwalk$SDP_option_match[OpenSDP.data:::xwalk$sdp_name == "iep"])
prog_baseline[, 1] <- as.character(prog_baseline[, 1])
prog_baseline[, 2] <- as.character(prog_baseline[, 2])
recode_options(prog_baseline[, 1:2], from = "SDP")
#
#   flat_name_list <- function(list){
#     char <- trimws(list[[1]])
#     names(char) <- trimws(list[[2]])
#     return(char)
#   }



# saveRDS(xwalk, "data/sdp_ceds_map.rds")
devtools::use_data(ell, xwalk, ses, internal = TRUE, overwrite = TRUE)
