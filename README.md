
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
#> 1   326   Male 1999-10-18                     Asian
#> 2   252   Male 1997-04-03                     White
#> 3   137   Male 1997-03-27                     White
#> 4   089   Male 1994-08-14                     White
#> 5   081   Male 1996-01-20 Black or African American
#> 6   246 Female 1992-11-21                     White
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
#> 1 326   Male 1999-10-18                     Asian    No
#> 2 252   Male 1997-04-03                     White   Yes
#> 3 137   Male 1997-03-27                     White   Yes
#> 4 089   Male 1994-08-14                     White   Yes
#> 5 081   Male 1996-01-20 Black or African American    No
#> 6 246 Female 1992-11-21                     White   Yes
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
#> 1 326 1999-10-18 2004 4.924059
#> 2 326 1999-10-18 2005 5.926027
#> 3 326 1999-10-18 2006 6.926027
#> 4 326 1999-10-18 2007 7.926027
#> 5 326 1999-10-18 2008 8.924059
#> 6 326 1999-10-18 2009 9.926027
```

ELL is a good example. A student's initial ELL status determines future ELL status to a high degree. To generate a student's ELL status over time, first the initial ELL status of a student needs to be set. Below, a dataframe of the first observation for each student in the dataset is created, this contains the `ID`, `year`, `age` in years, and `Race` of the student.

``` r
# Create ELL
### Initial
## Identify first enrollment period for a student
## Look up probability based on age/race of being ELL
## Assign student to ELL status or not in first year

stu_first <- stu_year %>% group_by(ID) %>% 
  mutate(flag = if_else(age == min(age), 1, 0)) %>% 
  filter(flag == 1) %>% select(-flag) %>% as.data.frame() %>% 
  select(ID, year, age)
stu_first <- inner_join(stu_first, demog_master[, c("ID", "Race")])
stu_first$age <- round(stu_first$age, 0)
head(stu_first)
#>    ID year age                      Race
#> 1 326 2004   5                     Asian
#> 2 252 2002   5                     White
#> 3 137 2002   5                     White
#> 4 089 1999   5                     White
#> 5 081 2001   6 Black or African American
#> 6 246 1997   5                     White
```

To assign students to inital ELL status, we need three things:

1.  A function that generates a random status ("ELL", "Not ELL")
2.  Parameters that define the probability of being in those two statuses
3.  A baseline of observed probabilities defined by those baselines

For ELL status, age and race are strong determinants of initial ELL status. Some racial groups are much more likely to be ELL and younger students are more likely to be ELL than older students.

The `OpenSDP.data` package bakes in some baseline values using `baseline` objects. A `baseline` object is a simple list with three elements:

1.  `keys` - the variable names that are required to match probabilities to cases, (e.g. age, race, etc.)
2.  `fun` - the function used to generate the student status
3.  `data` - the data that sets the parameters of the function

Let's look at the `baseline` for ELL, which can be accessed using the `get_baseline` function.

``` r
bl_data <- get_baseline("ell")
bl_data$keys
#> [1] "race" "age"
```

The keys are `race` and `age` -- to use this baseline we need data that includes the student `age` and `race`.

The function that will be used is `rbinom` and it will be passed one parameter, `x`.

``` r
bl_data$fun
#> function(x) rbinom(1, 1, x)
#> <environment: 0x000000000cdbed78>
```

The `bl_data$data` object tells us what the value of `x` will be:

``` r
head(bl_data$data)
#>   age  race  prob
#> 1   4 black 0.000
#> 2   5 black 0.000
#> 3   6 black 0.024
#> 4   7 black 0.032
#> 5   8 black 0.019
#> 6   9 black 0.017
```

For each combination of `age` and `race`, `rbinom` will be assigned a different probability, reflecting the empirical observed probability of being an ELL given the age and race provided.

Before we can use this baseline data, however, we need to ensure that the values `age` and `race` in our data match those in the `baseline`. We can check that this is not the case by comparing:

``` r
unique(bl_data$data$race)
#> [1] "black"       "asian"       "hispanic"    "amerind"     "white"      
#> [6] "other"       "multiracial" "hawaiian_pi" "total"
levels(stu_first$Race)
#> [1] "White"                                    
#> [2] "Hispanic or Latino Ethnicity"             
#> [3] "Black or African American"                
#> [4] "Asian"                                    
#> [5] "American Indian or Alaska Native"         
#> [6] "Native Hawaiian or Other Pacific Islander"
#> [7] "Demographic Race Two or More Races"
```

Our `stu_first` object is mapped to the CEDS specification. To convert it from CEDS to a more analyst friendly scheme, the `OpenSDP.data` package provides the `map_CEDS()` function.

``` r
# map_CEDS assigns a new vector, so put it in a new object
stu_first$race <- map_CEDS(stu_first$Race)
table(stu_first$race, stu_first$Race)[, 1:4]
#>              
#>               White Hispanic or Latino Ethnicity Black or African American
#>   amerind         0                            0                         0
#>   asian           0                            0                         0
#>   black           0                            0                        68
#>   hispanic        0                           79                         0
#>   multiracial     0                            0                         0
#>   white         318                            0                         0
#>              
#>               Asian
#>   amerind         0
#>   asian          22
#>   black           0
#>   hispanic        0
#>   multiracial     0
#>   white           0
```

With our data matching, we can now use the `assign_baseline()` function.

``` r
# Assign baseline creates a new vector, so assign it
stu_first$ell_first <- assign_baseline(baseline = "ell", data = stu_first)
# Recode it
stu_first$ell_first <- ifelse(stu_first$ell_first == 1, "Yes", "No")
head(stu_first)
#>    ID year age                      Race  race ell_first
#> 1 326 2004   5                     Asian asian       Yes
#> 2 252 2002   5                     White white        No
#> 3 137 2002   5                     White white        No
#> 4 089 1999   5                     White white        No
#> 5 081 2001   6 Black or African American black        No
#> 6 246 1997   5                     White white        No
```

Using the initial ELL status of students it is now possible to simulate the transition from ELL to non-ELL student.

To simulate this process, we can use a Markov chain defined by a transition matrix: <https://en.wikipedia.org/wiki/Examples_of_Markov_chains>

A transition matrix simply tabulates the number of times a vector transitions from one value to another. Given a student whose ELL status is defined as 0 = not ELL and 1 = ELL, with annual statuses given by:

    Student A:
    1 1 1 1 1 0 1 0 0 0

The transition matrix for this student is then:

| from/to | 0   | 1   |
|---------|-----|-----|
| 0       | 2   | 1   |
| 1       | 2   | 4   |

To construct a proper Markov transition matrix, this matrix needs to be converted to probabilities, that sum to 1 by rows.

| from/to | 0    | 1    |
|---------|------|------|
| 0       | 0.66 | 0.33 |
| 1       | 0.33 | 0.66 |

This can be read as:

-   For a student with ELL status 0, the probability of staying status 0 is 0.66, and the probability of switching to status 1 is 0.33
-   For a student with ELL status 1, the probability of switching to status 0 is 0.33, and the probability of staying status 1 is 0.66

Then, using this transition matrix, we can generate a sequence of enrollment patterns that fit this process. This approach has two advantages:

-   It generates believable transitions without requiring complex by-year conditional probabilities
-   It can be adapted to reflect the empirical transition matrix derived from a baseline of data

Let's look at an example. First, we combine the first observation for each student with the annual data.

``` r
stu_year <- left_join(stu_year, stu_first[, c(1, 6)])
head(stu_year)
#>    ID        DOB year      age ell_first
#> 1 326 1999-10-18 2004 4.924059       Yes
#> 2 326 1999-10-18 2005 5.926027       Yes
#> 3 326 1999-10-18 2006 6.926027       Yes
#> 4 326 1999-10-18 2007 7.926027       Yes
#> 5 326 1999-10-18 2008 8.924059       Yes
#> 6 326 1999-10-18 2009 9.926027       Yes
```

Now we define the transition matrix. Conveniently, we can input the observed pattern and then normalize it to a transition matrix by dividing it by the `rowSums()`.

``` r
# Define the transition matrix
statesNames <- c("No", "Yes")
tm <- matrix(c(800, 40, 120, 300), nrow = 2, byrow = TRUE,
             dimnames = list(statesNames, statesNames))
tm <- tm / rowSums(tm)
tm
#>            No        Yes
#> No  0.9523810 0.04761905
#> Yes 0.2857143 0.71428571
```

Now, for each student we need to apply the transition matrix. Using the `OpenSDP.data` function `make_markov_series()`, this is simple.

``` r
make_markov_series(10, tm = tm)
#>  [1] "No" "No" "No" "No" "No" "No" "No" "No" "No" "No"
```

And applying it to each student:

``` r
stu_year <- stu_year %>% 
  group_by(ID) %>% 
  arrange(ID, year) %>%
  mutate(ell = make_markov_series(n() - 1, 
          tm = tm, #define transition matrix
          t0 = ell_first[1], # specify that the matrix should start with first obs
          include.t0 = TRUE) # include the first observation in the sequence
         )

table(initialELL =stu_year$ell_first, byyear = stu_year$ell)
#>           byyear
#> initialELL   No  Yes
#>        No  4636  560
#>        Yes  565  239
```

Package Dependencies
--------------------

-   `dplyr`
-   `lubridate`
-   [wakefield](https://www.github.com/trinker/wakefield)

OpenSDP
-------

`OpenSDP.data` is part of the OpenSDP project.
