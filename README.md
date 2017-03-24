
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

Ljoad the package

``` r
library(OpenSDP.data)
```

``` r
out <- popsim_control(200, seed = 213)
#> Preparing student identities for 200 students...
#> Creating annual enrollment for 200 students...
#> Assigning 200 students to initial ELL status...
#> Assigning 200 students to initial SES status...
#> Organizing status variables for you...
#> Assigning 200 students longitudinal status trajectories...
#> Sorting your records
#> Cleaning up...
#> Success! Returning you student and student-year data in a list.
head(out$demog_master %>% arrange(sid) %>% select(1:4, ses))
#>   sid    Sex  Birthdate                         Race ses
#> 1 001 Female 1995-01-25 Hispanic or Latino Ethnicity Yes
#> 2 002 Female 1992-08-24                        White Yes
#> 3 003 Female 1992-05-17    Black or African American Yes
#> 4 004 Female 1997-04-01                        White  No
#> 5 005 Female 1992-06-16                        White  No
#> 6 006 Female 1995-08-29                        White  No
head(out$stu_year, 10)
#> Source: local data frame [10 x 7]
#> Groups: sid [1]
#> 
#>       sid  year   age   ses   ell   iep gifted
#>    <fctr> <int> <dbl> <chr> <chr> <chr>  <chr>
#> 1     001  1999     5   Yes    No   Yes     No
#> 2     001  2000     6   Yes    No   Yes     No
#> 3     001  2001     7   Yes    No   Yes     No
#> 4     001  2002     8   Yes    No   Yes     No
#> 5     001  2003     9   Yes    No   Yes     No
#> 6     001  2004    10    No    No   Yes     No
#> 7     001  2005    11    No    No   Yes     No
#> 8     001  2006    12    No    No   Yes     No
#> 9     001  2007    13    No    No   Yes     No
#> 10    001  2008    14    No    No   Yes     No
```

Parameters
----------

Default parameters can be modified by the user:

``` r
names(sim_control())
#> [1] "race_groups" "race_prob"   "minyear"     "maxyear"     "gifted_list"
#> [6] "iep_list"    "ses_list"    "ell_list"
sim_control()$ell_list
#> $ALL
#> $ALL$f
#> function (n, tm, ...) 
#> {
#>     stopifnot(is.matrix(tm))
#>     stopifnot(n > 0)
#>     if (any(tm > 1)) {
#>         warning("TM elements exceed 1, adjusting by dividing by rowSums")
#>         tm <- tm/rowSums(tm)
#>     }
#>     mc <- new("markovchain", transitionMatrix = tm)
#>     series <- markovchainSequence(n, mc, ...)
#>     return(series)
#> }
#> <environment: namespace:OpenSDP.data>
#> 
#> $ALL$pars
#> $ALL$pars$tm
#>            Yes         No
#> Yes 0.97560976 0.02439024
#> No  0.00621118 0.99378882
#> 
#> 
#> 
#> $GROUPVARS
#> [1] "ell"
```

These set some of the simulation requirements, but others are set using the `baseline` function family.

``` r
get_baseline("ses")
#> $data
#>          race  prob
#> 1       black 0.650
#> 2       asian 0.375
#> 3    hispanic 0.600
#> 4     amerind 0.400
#> 5       white 0.400
#> 6       other 0.400
#> 7 multiracial 0.400
#> 8 hawaiian_pi 0.400
#> 
#> $keys
#> [1] "race"
#> 
#> $fun
#> function (x) 
#> rbinom(1, 1, x)
#> <environment: 0x0000000010490920>
```

### Diagnostics

How do we know it worked? We can look at the patterns of ELL enrollment that are observed and see what patterns are the most common. To do this, let's compute the frequency of transition states observed per student.

``` r
library(ggplot2)
library(tidyr)
plotdf <- stu_year %>% arrange(sid, year) %>% group_by(sid) %>% 
  do(tidy_sequence(.$ell, states = c("Yes", "No")))

plotdf$total <- rowSums(plotdf[, -1])
plotdf <- plotdf %>% gather(-sid, key = "Transition", value = "Count")

# plotdf %>% group_by(Transition) %>% filter(Transition != "total") %>%
#   summarize(sum(Count))

plotdf <- plotdf %>% filter(Transition != "total")  %>% 
  group_by(sid) %>% 
  mutate(total = sum(Count)) %>% 
  mutate(per = Count / total) %>% filter(Transition != "total")  %>% 
  separate(Transition, into = c("From", "To"), sep = "-") 

ggplot(plotdf, aes(Count)) + geom_histogram() + 
  scale_x_continuous(breaks = c(0:25)) + 
  facet_grid(From~To, labeller = label_both, switch = "y") + 
  theme_bw() + 
  labs(title = "Frequency of Transition States by Student - ELL", 
       y = "Count", x = "Times per Student State Observed")
```

![](tools/figs/README-ellDiagnostic-1.png)

Looking at this chart we can see that most students went from the No state to a No state -- as would be expected when there are few ELLs.

Through this process we've gained students in the ELL status who were not initially ELL. Depending on our application this may not be desirable and we may want to modify the transition matrix to avoid this. Otherwise, later, this becomes an exercise in data cleaning.

Two other visual diagnostics are below.

``` r
# Other plots

# ggplot(plotdf, aes(per)) + geom_density() + 
#   facet_grid(From ~ To, labeller = label_both, switch = "y") + 
#   theme_bw() + labs(title = "By Student Densities of Transitions")

# Heatmap
plotdf %>% group_by(From, To) %>% 
  summarise(Count = sum(Count)) %>% 
  ungroup %>% 
  mutate(total = sum(Count)) %>%
  mutate(per = Count/total) %>%
ggplot(aes(x = From, y = To, fill = per)) + 
  geom_tile(color= I("black")) + 
  geom_text(aes(label = round(per, digits = 2))) + 
  theme_minimal() +
  coord_cartesian() + labs(title = "Heatmap of ELL Transition States")
```

![](tools/figs/README-visualdiagnostics-1.png)

We can also do a comparative diagnostic. Given the relatively short length of our sequence per student, it will be hard to estimate fit from a short sequence.

``` r
# series <- stu_year$ell[stu_year$ID == "1705"]
# series <- stu_year$ell[stu_year$ID == "0001"]

test_fit <- function(series, expected){
  if(dim(table(series)) == 1){
    return(TRUE)
  } else {
  out <- fit_series(series, return = "fit", confidencelevel = 0.99, 
                    possibleStates = rownames(expected))
  low <- out$lowerEndpointMatrix < expected
  hi <- out$upperEndpointMatrix > expected
  return(all(low, hi))
  }
}

defaultFit <- sim_control()$ell_list$ALL$pars$tm

test_res <- stu_year %>% group_by(sid) %>% 
  summarize(fit_ok = test_fit(ell, expected = defaultFit))

table(test_res$fit_ok)
#> 
#> FALSE  TRUE 
#>    28   172
```

Let's look at co-occurrence of status over time.

``` r
# Look at by year patterns of relationships by student year
table(FRL = stu_year$ses, GIFTED = stu_year$gifted)
#>      GIFTED
#> FRL     No  Yes
#>   No  1155  142
#>   Yes 2204  293
table(FRL = stu_year$ses, IEP = stu_year$iep)
#>      IEP
#> FRL     No  Yes
#>   No   783  514
#>   Yes 1437 1060
table(FRL = stu_year$gifted, IEP = stu_year$iep)
#>      IEP
#> FRL     No  Yes
#>   No  1987 1372
#>   Yes  233  202
```

Let's check polychoric correlations:

``` r
gamma_GK(stu_year$gifted, stu_year$iep)
#> $gamma
#> [1] 0.1133042
#> 
#> $se
#> [1] 0.07207095
#> 
#> $z
#> [1] 1.57212
#> 
#> $sig
#> [1] 0.1159226
gamma_GK(stu_year$ses, stu_year$iep)
#> $gamma
#> [1] 0.05824437
#> 
#> $se
#> [1] 0.049097
#> 
#> $z
#> [1] 1.186312
#> 
#> $sig
#> [1] 0.235499
gamma_GK(stu_year$ses, stu_year$ell)
#> $gamma
#> [1] 0.02424722
#> 
#> $se
#> [1] 0.06514072
#> 
#> $z
#> [1] 0.3722283
#> 
#> $sig
#> [1] 0.7097229
```

Finally, let's see who winds up "ever" in each category

``` r

test_df <- stu_year %>% group_by(sid) %>% 
  summarize(iep_ever = if_else(any(iep == "Yes"), "Yes", "No"), 
            ell_ever = if_else(any(ell == "Yes"), "Yes", "No"), 
            frpl_ever = if_else(any(ses == "Yes"), "Yes", "No"), 
            gifted_ever = if_else(any(gifted == "Yes"), "Yes", "No"))

table(IEP_EVER = test_df$iep_ever)
#> IEP_EVER
#>  No Yes 
#>  10 190
table(ELL_EVER = test_df$ell_ever)
#> ELL_EVER
#>  No Yes 
#> 152  48
table(FRPL_EVER = test_df$frpl_ever)
#> FRPL_EVER
#>  No Yes 
#>   1 199
table(GIFTED_EVER = test_df$gifted_ever)
#> GIFTED_EVER
#>  No Yes 
#> 167  33
```

Package Dependencies
--------------------

-   `dplyr`
-   `lubridate`
-   [wakefield](https://www.github.com/trinker/wakefield)

OpenSDP
-------

`OpenSDP.data` is part of the OpenSDP project.
