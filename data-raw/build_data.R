# Build data
library(purrr)
library(tidyr)

## Build example ELL baseline data
ell <- read.csv("data-raw/ellDist.csv")
ell <- na.omit(ell)
ell <-  ell %>% gather(-age, key = "race", value = "prob")

## Build example SDP/CEDS crosswalk
xwalk <- read.csv("data-raw/CEDS_SDP_map.csv", stringsAsFactors = FALSE)
xwalk$schema <- NA
for(i in 1:nrow(xwalk)){
  xwalk$schema[i] <- I(list(OpenSDP.data:::get_code_values(xwalk$CEDS_Option_set[i])))
}

# SES data
ses <- data.frame(race = c("black", "asian", "hispanic", "amerind", "white",
                           "other", "multiracial", "hawaiian_pi"),
                  prob = c(0.65, 0.375, 0.6, 0.4, 0.4, 0.4, 0.4, 0.4))
ses$race <- as.character(ses$race)

# Program Baseline
prog_baseline <- read.csv("data-raw/program_baseline.csv")
prog_baseline[, 1] <- as.character(prog_baseline[, 1])
prog_baseline[, 2] <- as.character(prog_baseline[, 2])
prog_baseline[, 3] <- as.character(prog_baseline[, 3])
names(prog_baseline) <- tolower(names(prog_baseline))
prog_baseline$frpl[prog_baseline$frpl == "2"] <- "1"
prog_baseline <- prog_baseline %>% group_by(ell, iep, frpl) %>%
  summarize(count = sum(count), prob = sum(prob)) %>% as.data.frame()
# Map CEDS names and option names
# prog_baseline[, 1:3] <- recode_options(prog_baseline[, 1:3], from = "SDP")
# names(prog_baseline)[1:3] <- map_CEDS(names(prog_baseline)[1:3])

## Pull in age_grade baseline
age_grade <- read.csv("data-raw/age_grade_baseline.csv")
names(age_grade) <- c("age", paste0("g", -1:12), "total")
age_grade[, 2:15] <- round(age_grade[, 2:15] / rowSums(age_grade[, 2:15]), 5)
age_grade$total <- NULL

# saveRDS(xwalk, "data/sdp_ceds_map.rds")
devtools::use_data(ell, xwalk, ses, prog_baseline, age_grade,
                   internal = TRUE, overwrite = TRUE)
