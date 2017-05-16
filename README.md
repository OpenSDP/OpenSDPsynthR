
<!-- README.md is generated from README.Rmd. Please edit that file -->
OpenSDP Data
============

A project to generate realistic synthetic unit-level longitudinal education data.

Design Goals
------------

1.  Generate synthetic education data that is realistic for use by analysts across the education sector. Realistic means messy, and reflective of the general pattern of relationships found in the U.S. education sector.
2.  Synthetic data should be able to be generated on-demand and responsive to inputs from the user. These inputs should allow the user to configure the process to produce data that resembles the patterns of data in their agency.
3.  The package should be modular and extendable allowing new data topics to be generated as needed so synthetic data coverage can grow.

Structure
---------

The package is organized into the following functions:

-   `simpop()` is the overall function that runs the simulation, this function calls many subfunctions to simulate different elements of the student data
-   `cleaners` are functions which take the output from the `simpop` function and reshape it into data formats for different analyses. Currently only two cleaners are supported -- `CEDS` and `sdp_cleaner()` which prepare the data into a CEDS like format and into the Strategic Data Project college-going analysis file specification respectively.
-   `sim_control()` -- a function that controls all of the parameters of the `simpop` simulation. The details of this function are covered in the vignettes.

Get Started
===========

To use `OpenSDP.data`, follow the instructions below:

Install Package
---------------

The development version of the package is able to be installed using the `install_github()`. To use this command you will need to install the `devtools` package.

``` r
devtools::install_github("strategicdataproject/OpenSDP.data")
```

Make some data
--------------

Load the package

``` r
library(OpenSDP.data)
#> Loading required package: dplyr
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
#> Loading required package: lme4
#> Loading required package: Matrix
```

The main function of the package is `simpop` which generates a list of data elements corresponding to simulated educational careers, K-20, for a user specified number of students. In R, a list is a data structure that can contain multiple data elements of different structures. This can be used to emulate the multiple tables of a Student Information System (SIS).

``` r
out <- simpop(nstu = 500, seed = 213, control = sim_control(nschls = 3))
#> Preparing student identities for 500 students...
#> Creating annual enrollment for 500 students...
#> Assigning 500 students to initial FRPL, IEP, and ELL status
#> Assigning initial grade levels...
#> Organizing status variables for you...
#> Assigning 500 students longitudinal status trajectories...
#> Sorting your records
#> Cleaning up...
#> Creating 3 schools for you...
#> Assigning 6946 student-school enrollment spells...
#> Simulating assessment table... be patient...
#> Simulating high school outcomes... be patient...
#> Simulating annual high school outcomes... be patient...
#> Simulating postsecondary outcomes... be patient...
#> Success! Returning you student and student-year data in a list.
```

Currently ten tables are produced:

``` r
names(out)
#>  [1] "demog_master" "stu_year"     "schools"      "stu_assess"  
#>  [5] "hs_outcomes"  "hs_annual"    "nsc"          "ps_enroll"   
#>  [9] "assessments"  "proficiency"
```

The table below shows the data elements available in each table:

<!--html_preserve-->

<script type="application/json" data-for="htmlwidget-b7bfc5640e38922740fd">{"x":{"filter":"bottom","filterHTML":"<tr>\n  <td><\/td>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","caption":"<caption>Simulation Data Elements by Table<\/caption>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137"],["demog_master","demog_master","demog_master","demog_master","demog_master","demog_master","demog_master","demog_master","demog_master","demog_master","demog_master","demog_master","stu_year","stu_year","stu_year","stu_year","stu_year","stu_year","stu_year","stu_year","stu_year","stu_year","stu_year","stu_year","stu_year","stu_year","stu_year","stu_year","stu_year","schools","schools","schools","schools","schools","schools","schools","schools","schools","schools","schools","schools","schools","schools","stu_assess","stu_assess","stu_assess","stu_assess","stu_assess","stu_assess","stu_assess","hs_outcomes","hs_outcomes","hs_outcomes","hs_outcomes","hs_outcomes","hs_outcomes","hs_outcomes","hs_outcomes","hs_outcomes","hs_outcomes","hs_outcomes","hs_annual","hs_annual","hs_annual","hs_annual","hs_annual","hs_annual","hs_annual","hs_annual","hs_annual","hs_annual","hs_annual","hs_annual","hs_annual","hs_annual","hs_annual","hs_annual","hs_annual","hs_annual","hs_annual","hs_annual","hs_annual","nsc","nsc","nsc","nsc","nsc","nsc","nsc","nsc","nsc","nsc","nsc","nsc","nsc","nsc","nsc","nsc","nsc","nsc","nsc","nsc","nsc","nsc","nsc","nsc","nsc","nsc","ps_enroll","ps_enroll","ps_enroll","ps_enroll","ps_enroll","ps_enroll","ps_enroll","ps_enroll","ps_enroll","ps_enroll","ps_enroll","ps_enroll","assessments","assessments","assessments","assessments","assessments","assessments","assessments","assessments","assessments","assessments","proficiency","proficiency","proficiency","proficiency","proficiency","proficiency","proficiency"],["sid","Sex","Birthdate","Race","White","Asian","American.Indian.or.Alaska.Native","Black.or.African.American","Hispanic.or.Latino.Ethnicity","Demographic.Race.Two.or.More.Races","Native.Hawaiian.or.Other.Pacific.Islander","id_type","sid","year","age","grade","frpl","ell","iep","gifted","grade_advance","cohort_year","cohort_grad_year","exit_type","enrollment_status","ndays_possible","ndays_attend","att_rate","schid","schid","name","enroll","male_per","frpl_per","sped_per","lep_per","gifted_per","lea_id","id_type","title1_status","title3_program_type","type","poverty_desig","sid","schid","year","grade","math_ss","rdg_ss","grade_enrolled","sid","scale_gpa","gpa","grad_prob","grad","hs_status","ps_prob","ps","chrt_grad","class_rank","diploma_type","sid","grad","hs_status","chrt_grad","class_rank","diploma_type","year","grade","schid","yr_seq","ontrack","cum_credits","cum_credits_ela","cum_credits_math","cum_gpa","credits_earned","credits_attempted","cum_credits_attempted","expected_grad_hs","grad_cohort_ind","status_after","city","state","name","online_only","avg_net_price_pub","pell_grant_rate","retention_four_year_full_time","part_time_share","act_25th_pctl_cumulative","act_75th_pctl_cumulative","sat_average_all","sat_25th_pctl_math","sat_75th_pctl_math","sat_25th_pctl_reading","sat_75th_pctl_reading","sat_25th_pctl_writing","sat_75th_pctl_writing","race_ethn_white","race_ethn_black","race_ethn_hispanic","race_ethn_asian","race_ethn_two_or_more","opeid","short_name","enroll","type","sid","year","term","ps_prob","grad","gpa","ps","yr_seq","ps_transfer","opeid","ps_short_name","ps_type","sid","schid","year","grade","subject","score","score_type","assess_id","assess_name","retest_ind","year","grade","subject","assess_id","score_mean","score_error","ntests"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>table<\/th>\n      <th>column<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":12,"autoWidth":true,"searching":true,"autoFill":true,"dom":"lrtp","order":[],"orderClasses":false,"columnDefs":[{"orderable":false,"targets":0}],"lengthMenu":[10,12,25,50,100]}},"evals":[],"jsHooks":[]}</script>
<!--/html_preserve-->
``` r
head(out$demog_master %>% arrange(sid) %>% select(1:4))
#>   sid    Sex  Birthdate                         Race
#> 1 001   Male 2000-01-30                        White
#> 2 002   Male 1998-01-25                        White
#> 3 003 Female 2000-10-22    Black or African American
#> 4 004 Female 2003-10-05                        White
#> 5 005 Female 2001-05-27 Hispanic or Latino Ethnicity
#> 6 006 Female 1998-12-16                        White
head(out$stu_year, 10)
#> Source: local data frame [10 x 17]
#> Groups: sid [10]
#> 
#>       sid  year   age grade  frpl   ell   iep gifted grade_advance
#>    <fctr> <dbl> <dbl> <chr> <chr> <chr> <chr>  <chr>         <chr>
#> 1     002  2002     5    KG     0     0     0      0          <NA>
#> 2     009  2002     6     1     1     0     0      1          <NA>
#> 3     014  2002     5    KG     1     0     0      0          <NA>
#> 4     017  2002     4    PK     0     0     0      0          <NA>
#> 5     023  2002     5    KG     0     0     0      0          <NA>
#> 6     024  2002     4    PK     0     0     0      0          <NA>
#> 7     028  2002     5    KG     0     0     0      1          <NA>
#> 8     030  2002     5    KG     0     0     0      0          <NA>
#> 9     031  2002     6     1     1     0     0      0          <NA>
#> 10    034  2002     5    KG     1     0     0      0          <NA>
#> # ... with 8 more variables: cohort_year <dbl>, cohort_grad_year <dbl>,
#> #   exit_type <lgl>, enrollment_status <chr>, ndays_possible <dbl>,
#> #   ndays_attend <dbl>, att_rate <dbl>, schid <chr>
```

Parameters
----------

Default parameters can be modified by the user:

``` r
names(sim_control())
#>  [1] "nschls"                "best_schl"            
#>  [3] "race_groups"           "race_prob"            
#>  [5] "minyear"               "maxyear"              
#>  [7] "gifted_list"           "iep_list"             
#>  [9] "ses_list"              "ell_list"             
#> [11] "ps_transfer_list"      "grade_levels"         
#> [13] "n_cohorts"             "school_means"         
#> [15] "school_cov_mat"        "school_names"         
#> [17] "postsec_names"         "gpa_sim_parameters"   
#> [19] "grad_sim_parameters"   "ps_sim_parameters"    
#> [21] "assess_sim_par"        "assessment_adjustment"
#> [23] "grad_adjustment"       "ps_adjustment"        
#> [25] "gpa_adjustment"        "assess_grades"
sim_control()$ell_list
#> $ALL
#> $ALL$f
#> function (n, tm, burnin = NULL, ...) 
#> {
#>     stopifnot(is.matrix(tm))
#>     stopifnot(n > 0)
#>     if (any(tm > 1)) {
#>         warning("TM elements exceed 1, adjusting by dividing by rowSums")
#>         tm <- tm/rowSums(tm)
#>     }
#>     mc <- new("markovchain", transitionMatrix = tm)
#>     if (!is.null(burnin)) {
#>         series <- markovchainSequence(n + burnin, mc, ...)
#>         series <- series[burnin:(n + burnin)]
#>     }
#>     else {
#>         series <- markovchainSequence(n, mc, ...)
#>     }
#>     return(series)
#> }
#> <environment: namespace:OpenSDP.data>
#> 
#> $ALL$pars
#> $ALL$pars$tm
#>            1          0
#> 1 0.97560976 0.02439024
#> 0 0.00621118 0.99378882
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
#> <environment: 0x000000000d6c46b8>
```

### Diagnostics

How do we know it worked? We can look at the patterns of ELL enrollment that are observed and see what patterns are the most common. To do this, let's compute the frequency of transition states observed per student.

``` r
library(ggplot2)
library(tidyr)
plotdf <- stu_year %>% arrange(sid, year) %>% group_by(sid) %>% 
  do(tidy_sequence(.$ell, states = c(1, 0)))

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
#>    37   463
```

Let's look at co-occurrence of status over time.

``` r
# Look at by year patterns of relationships by student year
table(FRL = stu_year$frpl, GIFTED = stu_year$gifted)
#>    GIFTED
#> FRL    0    1
#>   0 3845  524
#>   1 2299  278
table(FRL = stu_year$frpl, IEP = stu_year$iep)
#>    IEP
#> FRL    0    1
#>   0 3612  757
#>   1 2054  523
table(GIFTED = stu_year$gifted, IEP = stu_year$iep)
#>       IEP
#> GIFTED    0    1
#>      0 5070 1074
#>      1  596  206
```

Let's check polychoric correlations:

``` r
gamma_GK(stu_year$gifted, stu_year$iep)
#> $gamma
#> [1] 0.240018
#> 
#> $se
#> [1] 0.06233679
#> 
#> $z
#> [1] 3.850342
#> 
#> $sig
#> [1] 0.0001179531
gamma_GK(stu_year$frpl, stu_year$iep)
#> $gamma
#> [1] 0.09703904
#> 
#> $se
#> [1] 0.0446976
#> 
#> $z
#> [1] 2.171012
#> 
#> $sig
#> [1] 0.02993025
gamma_GK(stu_year$frpl, stu_year$ell)
#> $gamma
#> [1] 0.195661
#> 
#> $se
#> [1] 0.06521843
#> 
#> $z
#> [1] 3.000088
#> 
#> $sig
#> [1] 0.002699016
```

Finally, let's see who winds up "ever" in each category

``` r

test_df <- stu_year %>% group_by(sid) %>% 
  summarize(iep_ever = if_else(any(iep == 1), "Yes", "No"), 
            ell_ever = if_else(any(ell == 1), "Yes", "No"), 
            frpl_ever = if_else(any(frpl == 1), "Yes", "No"), 
            gifted_ever = if_else(any(gifted == 1), "Yes", "No"))

table(IEP_EVER = test_df$iep_ever)
#> IEP_EVER
#>  No Yes 
#> 367 133
table(ELL_EVER = test_df$ell_ever)
#> ELL_EVER
#>  No Yes 
#> 441  59
table(FRPL_EVER = test_df$frpl_ever)
#> FRPL_EVER
#>  No Yes 
#> 128 372
table(GIFTED_EVER = test_df$gifted_ever)
#> GIFTED_EVER
#>  No Yes 
#> 426  74
```

Assigning Schools and Outcomes
------------------------------

Students move through grades, schools, and outcomes.

Package Dependencies
--------------------

-   `dplyr`
-   `lubridate`
-   [wakefield](https://www.github.com/trinker/wakefield)

OpenSDP
-------

`OpenSDP.data` is part of the OpenSDP project.
