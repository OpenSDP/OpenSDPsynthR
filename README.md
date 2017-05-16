
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

Data elements produced include:

-   **Student demographics:** age, race, and sex
-   **Student participation:** grade advancement, ELL status, IEP status, FRPL status, gifted and talented status, attendance
-   **Student enrollment status:** exit type, enrollment type, transfer, graduation, dropout, etc.
-   **School attributes:** name, school category, school size, Title I and Title III status, etc.
-   **Student assessment:** math assessment, reading assessment, grade level assessed
-   **High school outcomes:** graduation, cumulative GPA, graduation type, cohort, class rank, postsecondary enrollment
-   **High school progression:** annual class rank, cumulative credits earned, credits earned, credits by English Language Arts and by Mathematics, credits attempted, ontrack status
-   **Postsecondary enrollment:** year of enrollment, transfer indicator, name and ID of postsecondary institution, type of institution
-   \*\*Postsecondary institution <data:**> name, city, state, online only, average net price, Pell grant rate, retention four year full time, share of part time enrollment, enrollment by race, SAT and ACT score distribution for admitted students

There are two tables of metadata about the assessment data above to be used in cases where multiple types of student assessment are analyzed together.

-   **Assessment information:** grade, subject, ID, type, and name of assessment
-   **Proficiency information:** mean score, error of score, number of students tested

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
#> <environment: 0x0000000006047050>
```

Package Dependencies
--------------------

-   `dplyr`
-   `lubridate`
-   [wakefield](https://www.github.com/trinker/wakefield)

OpenSDP
-------

`OpenSDP.data` is part of the OpenSDP project.
