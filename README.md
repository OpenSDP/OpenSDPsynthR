
<!-- README.md is generated from README.Rmd. Please edit that file -->
OpenSDP Data
============

A project to generate realistic synthetic unit-level longitudinal education data.

Design Goals
------------

1.  Generate synthetic education data that is realistic for use by analysts across the education sector. Realistic means messy, and reflective of the general pattern of relationships found in the U.S. education sector.
2.  Synthetic data should be able to be generated on-demand and responsive to inputs from the user. These inputs should allow the user to configure the process to produce data that resembles the patterns of data in their agency.
3.  The package should be modular and extendable allowing new data topics to be generated as needed so synthetic data coverage can grow.

Get Started
===========

To use `OpenSDP.data`, follow the instructions below:

Install Package
---------------

``` r
devtools::install_github("strategicdataproject/OpenSDP.data")
```

Make some data
--------------

Using the `wakefield` package we can generate a simple set of demographic data.

``` r
library(OpenSDP.data)
library(magrittr)
library(wakefield)
library(lubridate)
set.seed(612)

demog_master <- r_data_frame(n = 500, 
                             id(random = TRUE), 
                             sex, 
                             # dob, set range of years available for birth
                             dob(start = Sys.Date() - 365 * 25, 
                                 k = 365 * 8, by = "1 days"), 
                             race(x = c("White", "Hispanic or Latino Ethnicity", 
                                        "Black or African American", 
                                        "Asian", "American Indian or Alaska Native", 
                                        "Native Hawaiian or Other Pacific Islander", 
                                        "Demographic Race Two or More Races"), 
                                  prob = c(0.637, 0.163, 0.122, 0.047, .007, .0015, .021)))

head(demog_master)
#> # A tibble: 6 × 4
#>      ID    Sex        DOB                      Race
#>   <chr> <fctr>     <date>                    <fctr>
#> 1   326   Male 1999-10-17                     Asian
#> 2   252   Male 1997-04-02                     White
#> 3   137   Male 1997-03-26                     White
#> 4   089   Male 1994-08-13                     White
#> 5   081   Male 1996-01-19 Black or African American
#> 6   246 Female 1992-11-20                     White
```

Next, let's break the "Race" variable into a series of indicator variables.

``` r
demog_master %<>% make_inds("Race")
demog_master %<>% mutate_at(5:11, 
                        funs(recode(., `0` = "No", `1` = "Yes")))
head(demog_master[, 4:9])
#>                        Race White Hispanic.or.Latino.Ethnicity
#> 1                     Asian    No                           No
#> 2                     White   Yes                           No
#> 3                     White   Yes                           No
#> 4                     White   Yes                           No
#> 5 Black or African American    No                           No
#> 6                     White   Yes                           No
#>   Black.or.African.American Asian American.Indian.or.Alaska.Native
#> 1                        No   Yes                               No
#> 2                        No    No                               No
#> 3                        No    No                               No
#> 4                        No    No                               No
#> 5                       Yes    No                               No
#> 6                        No    No                               No
```

Now, let's generate some variables conditional on `race`. To do this we build a list that defines the distribution of this new variable for each category of the factor level.

``` r
# List of conditional probabilties
ses_list <- list("White" = list(f = rnorm, 
                                pars = list(mean = 0.3, sd = 1.1)), 
                 "Hispanic or Latino Ethnicity" = list(f = rnorm, 
                                pars = list(mean = -0.1, sd = 0.9)),
                 "Black or African American" = list(f = rnorm, 
                                pars = list(mean = -0.2, sd = 1.2)), 
                    "Asian" = list(f = rnorm, 
                                pars = list(mean = 0.23, sd = 1.2)), 
                 "Demographic Race Two or More Races" = list(f = rnorm, 
                                pars = list(mean = 0.0, sd = 1)), 
                 "American Indian or Alaska Native" = list(f = rnorm, 
                                pars = list(mean = -0.2, sd = 1)), 
                    "Other" = list(f = rnorm, 
                                pars = list(mean = 0, sd = 1)),
                 "Native Hawaiian or Other Pacific Islander" = list(f = rnorm, 
                                pars = list(mean = 0, sd = 1))
                    )

ses_list_b <- list("White" = list(f = rbinom, 
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
                                  pars = list(size = 1, prob = 0.4))
)

# Note that cond_prob returns the whole data object
demog_master <- as.data.frame(demog_master)
demog_master <- cond_prob(demog_master, factor = "Race", 
                 newvar = "ses", prob_list = ses_list_b)

head(demog_master)
#>    ID    Sex        DOB                      Race White
#> 1 326   Male 1999-10-17                     Asian    No
#> 2 252   Male 1997-04-02                     White   Yes
#> 3 137   Male 1997-03-26                     White   Yes
#> 4 089   Male 1994-08-13                     White   Yes
#> 5 081   Male 1996-01-19 Black or African American    No
#> 6 246 Female 1992-11-20                     White   Yes
#>   Hispanic.or.Latino.Ethnicity Black.or.African.American Asian
#> 1                           No                        No   Yes
#> 2                           No                        No    No
#> 3                           No                        No    No
#> 4                           No                        No    No
#> 5                           No                       Yes    No
#> 6                           No                        No    No
#>   American.Indian.or.Alaska.Native
#> 1                               No
#> 2                               No
#> 3                               No
#> 4                               No
#> 5                               No
#> 6                               No
#>   Native.Hawaiian.or.Other.Pacific.Islander
#> 1                                        No
#> 2                                        No
#> 3                                        No
#> 4                                        No
#> 5                                        No
#> 6                                        No
#>   Demographic.Race.Two.or.More.Races ses
#> 1                                 No   0
#> 2                                 No   0
#> 3                                 No   0
#> 4                                 No   0
#> 5                                 No   1
#> 6                                 No   1
```

Now we have basic individual demographics, let's add annual attributes.

``` r
## Generate student-year data
minyear <- 1997
maxyear <- 2016
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

head(stu_year)
#>    ID        DOB year      age
#> 1 326 1999-10-17 2004 4.926798
#> 2 326 1999-10-17 2005 5.928767
#> 3 326 1999-10-17 2006 6.928767
#> 4 326 1999-10-17 2007 7.928767
#> 5 326 1999-10-17 2008 8.926798
#> 6 326 1999-10-17 2009 9.928767
```

Create an ELL indicator:

``` r
# Create ELL
### Initial
## Identify first enrollment period for a student
## Look up probability based on age/race of being ELL
## Assign student to ELL status or not in first year

### Longitudinal
## If a student is not ELL, give a very very low probability of being ELL in the 
## future (.0001 in t + 1, .000001 in t+n)
## If a student is ELL, define a function for probability of exiting ELL status

stu_first <- stu_year %>% group_by(ID) %>% 
  mutate(flag = if_else(age == min(age), 1, 0)) %>% 
  filter(flag == 1) %>% select(-flag) %>% as.data.frame()

stu_first <- inner_join(stu_first, demog_master[, c("ID", "Race")])
stu_first$age <- round(stu_first$age, 0)
stu_first$race <- map_CEDS(stu_first$Race)
stu_first$ell_first <- assign_baseline(baseline = "ell", data = stu_first)
stu_first$ell_first <- ifelse(stu_first$ell_first == 1, "Yes", "No")
head(stu_first)
#>    ID        DOB year age                      Race  race ell_first
#> 1 326 1999-10-17 2004   5                     Asian asian       Yes
#> 2 252 1997-04-02 2002   5                     White white        No
#> 3 137 1997-03-26 2002   5                     White white        No
#> 4 089 1994-08-13 1999   5                     White white        No
#> 5 081 1996-01-19 2001   6 Black or African American black        No
#> 6 246 1992-11-20 1997   5                     White white        No
```

``` r
stu_year <- left_join(stu_year, stu_first[, c(1, 7)])
# Option to use start frequencies as basis for chains or generate chains without 
# initial states
# Define the transition frequencies
statesNames <- c("No", "Yes")
tm <- matrix(c(800, 40, 120, 300), nrow = 2, byrow = TRUE, 
             dimnames = list(statesNames, statesNames))

# frpl
tm_f <- matrix(c(900, 200, 300, 2000), nrow = 2, byrow=TRUE, 
               dimnames = list(statesNames, statesNames))
make_markov_series(20, tm = tm_f/rowSums(tm_f))

# gifted
tm_g <- matrix(c(2000, 200, 20, 400), nrow = 2, byrow=TRUE, 
               dimnames = list(statesNames, statesNames))
make_markov_series(20, tm = tm_g/rowSums(tm_g))
# iep
tm_i <- matrix(c(3000, 200, 200, 3000), nrow = 2, byrow=TRUE, 
               dimnames = list(statesNames, statesNames))
make_markov_series(20, tm = tm_i/rowSums(tm_i))

# grade_level
gradeNames <- c("1", "0", "-1")
tm_grade <- matrix(c(900, 50, 10, 900, 20, 10, 900, 20, 10), nrow = 3, byrow=TRUE, 
               dimnames = list(gradeNames, gradeNames))
testMC <- as(tm_grade/rowSums(tm_grade), "markovchain")

make_markov_series(20, tm = tm_grade/rowSums(tm_grade), t0 = "1")


stu_year %<>% group_by(ID) %>% arrange(ID, year) %>% 
  mutate(ell = make_markov_series(n(), tm = tm/rowSums(tm), 
                                  t0 = ell_first[1], 
                                  include.t0 = TRUE))

stu_year %<>% group_by(ID) %>% arrange(ID, year) %>% 
  mutate(frpl = make_markov_series(n(), tm = tm_f/rowSums(tm_f)), 
         gifted = make_markov_series(n(), tm = tm_g/rowSums(tm_g)), 
         iep = make_markov_series(n(), tm = tm_i/rowSums(tm_i)), 
         grade_adv = make_markov_series(n(), tm = tm_grade/rowSums(tm_grade)))
```

``` r
# Conditional markovchains
tm_f <- matrix(c(900, 200, 300, 2000), nrow = 2, byrow=TRUE, 
               dimnames = list(statesNames, statesNames))
tm_f <- matrix(c(900, 200, 300, 2000), nrow = 2, byrow=TRUE, 
               dimnames = list(statesNames, statesNames))

make_markov_series(20, tm = tm_f/rowSums(tm_f))



tm_list <- replicate(8, matrix(c(sample(750:900, 1),
                    sample(400:500, 1),
                    sample(125:175, 1),
                    sample(1500:2200, 1)),
                    2, 2, dimnames = list(c("Yes", "No"), 
                    c("Yes", "No"))), simplify = FALSE) %>% lapply(function(x) x / rowSums(x))


ses_list_MC <- list("White" = list(f = make_markov_series, 
                                pars = list(tm = tm_list[[1]])), 
                 "Hispanic or Latino Ethnicity" = list(f = make_markov_series, 
                                pars = list(tm = tm_list[[2]])),
                 "Black or African American" = list(f = make_markov_series, 
                                pars = list(tm = tm_list[[3]])),
                 "Asian" = list(f = make_markov_series, 
                                pars = list(tm = tm_list[[4]])), 
                 "Demographic Race Two or More Races" = list(f = make_markov_series, 
                                pars = list(tm = tm_list[[5]])), 
                 "American Indian or Alaska Native" = list(f = make_markov_series, 
                                pars = list(tm = tm_list[[6]])), 
                 "Other" = list(f = make_markov_series, 
                                pars = list(tm = tm_list[[7]])),
                 "Native Hawaiian or Other Pacific Islander" = list(f = make_markov_series, 
                                pars = list(tm = tm_list[[8]])))

tm_grade_f <- tm_grade/rowSums(tm_grade)
tm_grade_m <- tm_grade
tm_grade_m[, 2] <- tm_grade_m[, 2] + 30
tm_grade_m[, 3] <- tm_grade_m[, 3] + 5
tm_grade_m <- tm_grade_m/rowSums(tm_grade_m)

retention_list <- list("Male" = list(f = make_markov_series, 
                                     pars = list(tm = tm_grade_m)),
                       "Female" = list(f = make_markov_series, 
                                       pars = list(tm_grade_f)))

testDF <- left_join(stu_year[, c(1:4)], demog_master[, c(1, 2, 4)])

testDF <- as.data.frame(testDF)
testDF <- cond_prob(testDF, factor = "Race", 
                 newvar = "frpl", prob_list = ses_list_MC)
testDF <- cond_prob(testDF, factor = "Sex", 
                 newvar = "grade_adv", prob_list = retention_list)

testDF %>% slice_rows("ID") %>% 
  by_slice(~ createSequenceMatrix(.x$grade_adv, possibleStates = c("-1", "0", "1"))) 


testDF %>% group_by(Sex) %>%
  split(.$ID) %>% select(grade_adv) %>% 
  map(~ createSequenceMatrix(possibleStates = c("-1", "0", "1"))) %>% 
  reduce(`+`)


testDF %>% split(.$ID) %>% 
  bind_rows() %>% 
  group_by(Sex) %>%
  summarise_each(sum)

zed <- testDF %>%
  group_by(Sex) %>% 
  slice_rows("ID") %>% 
  select(grade_adv, Sex) %>% 
  by_slice(~ createSequenceMatrix(., possibleStates = c("-1", "0", "1")), .labels =TRUE) %>% 
  bind_rows(.$.out) %>%
  group_by(Sex) %>% 
  reduce(`+`)
  
zed <- testDF %>% group_by(Sex, ID) %>% 
  do(.out = createSequenceMatrix(.$grade_adv, possibleStates = c("-1", "0", "1"))) %>% 
  ungroup %>%
  group_by(Sex) %>% 
  



testDF %>% 
  split(.$ID) %>% 
  by_slice(~ createSequenceMatrix(.x$grade_adv, possibleStates = c("-1", "0", "1"))) %>% 
  reduce(`+`)

testDF %>%
  slice_rows("ID") %>%
  nest() %>% 
  mutate(.out = map(.$data, partial(lm, mpg ~ disp)))


# Build diagnostics to ensure things are different by group
```

``` r
emp_tm <- out %>% group_by(ell_first) %>% split(.$ID) %>% 
  map(~ createSequenceMatrix(.x$ell, possibleStates = c("Yes", "No"))) %>% 
  reduce(`+`)

emp_tm <- out %>% split(.$ID) %>% 
  map(~ createSequenceMatrix(.x$ell, possibleStates = c("Yes", "No"))) %>% 
  reduce(`+`)

library(tidyr); library(viridis)
stu_year %>% split(.$ID) %>% 
  map(~ createSequenceMatrix(.x$grade_adv, possibleStates = c("-1", "0", "1"))) %>% 
  reduce(`+`) %>% tidy() %>% 
  gather(2:ncol(.), key = "to", value = "n") %>% 
  rename(from = .rownames) %>% 
  ggplot(aes(x = from, y = to, fill = n)) + geom_tile() + 
  scale_fill_viridis(direction = -1) + geom_text(aes(label = n))



ggplot(stu_year[stu_year$ID %in% unique(stu_year$ID)[1:50],], 
       aes(x = year, y = as.numeric(factor(frpl)), 
       group = ID)) + 
  geom_step(stat = "identity", alpha = I(0.1)) + 
  scale_y_discrete("FRPL", breaks = c(1, 2), labels = c("No", "Yes")) + 
  scale_x_continuous(breaks = c(1998:2017))


ggplot(out, aes(x = year, y = as.numeric(factor(ell)))) + 
  geom_step(stat = "identity") + 
  theme_bw() + 
  scale_y_discrete(name = "State", breaks = c(1, 2), 
                   labels = c("No", "Yes"))
```

Package Dependencies
--------------------

-   `dplyr`
-   `lubridate`
-   [wakefield](https://www.github.com/trinker/wakefield)

OpenSDP
-------

`OpenSDP.data` is part of the OpenSDP project.
