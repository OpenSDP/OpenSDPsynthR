
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

By default, you only need to specify the number of students to simulate to the `simpop` command. The package has default simulation parameters that will result in creating a small school district with two schools.

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
```

These parameters can have complex structures to allow for conditional and random generation of data. Parameters fall into four categories:

-   vectors: a single list of parameters like school names, category names, or school IDs
-   conditional probability lists:
-   outcome simulation parameters
-   outcome adjustments:

### Baselines

Currently there are two special parameters that are set based on baseline data built into the package. These are the initial grade distribution of students, and the initial program participation of students in `ell`, `iep`, and `frpl` programs.

These set some of the simulation requirements, but others are set using the `baseline` function family.

``` r
get_baseline("program")
#> $data
#>   ell iep frpl count  prob
#> 1   0   0    0 56838 55.00
#> 2   0   0    1 46104 29.63
#> 3   0   1    0  8178  4.44
#> 4   0   1    1 11423  6.13
#> 5   1   0    0  1089  0.64
#> 6   1   0    1  4771  3.59
#> 7   1   1    0   121  0.09
#> 8   1   1    1   871  0.48
#> 
#> $keys
#> NULL
#> 
#> $fun
#> function () 
#> {
#>     prog_baseline[sample(rownames(prog_baseline), 1, prob = prog_baseline$prob), 
#>         1:3]
#> }
#> <environment: 0x000000001065fcd0>
get_baseline("grade")
#> $data
#>    age     g-1      g0      g1      g2      g3      g4      g5      g6
#> 1    0 0.20000 0.60000 0.00000 0.00000 0.00000 0.00000 0.20000 0.00000
#> 2    1 0.80000 0.10000 0.10000 0.00000 0.00000 0.00000 0.00000 0.00000
#> 3    2 1.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000
#> 4    3 0.99647 0.00353 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000
#> 5    4 0.95125 0.04875 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000
#> 6    5 0.01327 0.97812 0.00850 0.00011 0.00000 0.00000 0.00000 0.00000
#> 7    6 0.00000 0.11595 0.87473 0.00921 0.00010 0.00000 0.00000 0.00000
#> 8    7 0.00000 0.00122 0.16272 0.82616 0.00990 0.00000 0.00000 0.00000
#> 9    8 0.00000 0.00011 0.00368 0.19658 0.78788 0.01166 0.00011 0.00000
#> 10   9 0.00000 0.00010 0.00021 0.00675 0.21692 0.76160 0.01443 0.00000
#> 11  10 0.00000 0.00000 0.00000 0.00000 0.01061 0.22371 0.74987 0.01561
#> 12  11 0.00000 0.00000 0.00000 0.00000 0.00000 0.01584 0.23237 0.73439
#> 13  12 0.00000 0.00000 0.00000 0.00000 0.00010 0.00021 0.01651 0.24751
#> 14  13 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00011 0.02236
#> 15  14 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00062
#> 16  15 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000
#> 17  16 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000
#> 18  17 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000
#> 19  18 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000
#> 20  19 0.00000 0.00000 0.00000 0.00207 0.00000 0.00000 0.00000 0.00000
#> 21  20 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00606
#> 22  21 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000
#> 23  22 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000
#>         g7      g8      g9     g10     g11     g12
#> 1  0.00000 0.00000 0.00000 0.00000 0.00000 0.00000
#> 2  0.00000 0.00000 0.00000 0.00000 0.00000 0.00000
#> 3  0.00000 0.00000 0.00000 0.00000 0.00000 0.00000
#> 4  0.00000 0.00000 0.00000 0.00000 0.00000 0.00000
#> 5  0.00000 0.00000 0.00000 0.00000 0.00000 0.00000
#> 6  0.00000 0.00000 0.00000 0.00000 0.00000 0.00000
#> 7  0.00000 0.00000 0.00000 0.00000 0.00000 0.00000
#> 8  0.00000 0.00000 0.00000 0.00000 0.00000 0.00000
#> 9  0.00000 0.00000 0.00000 0.00000 0.00000 0.00000
#> 10 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000
#> 11 0.00020 0.00000 0.00000 0.00000 0.00000 0.00000
#> 12 0.01719 0.00010 0.00000 0.00000 0.00000 0.00010
#> 13 0.71242 0.02305 0.00010 0.00000 0.00000 0.00010
#> 14 0.25146 0.69757 0.02829 0.00000 0.00000 0.00021
#> 15 0.03221 0.24816 0.69531 0.02369 0.00000 0.00000
#> 16 0.00051 0.03064 0.31636 0.62790 0.02388 0.00072
#> 17 0.00000 0.00111 0.10026 0.27668 0.59653 0.02542
#> 18 0.00000 0.00000 0.03076 0.08598 0.25533 0.62793
#> 19 0.00000 0.00000 0.02625 0.07837 0.17745 0.71793
#> 20 0.00000 0.00000 0.02490 0.06846 0.19295 0.71162
#> 21 0.00000 0.00000 0.04242 0.07273 0.11515 0.76364
#> 22 0.00000 0.00000 0.12000 0.04000 0.32000 0.52000
#> 23 0.00000 0.00000 0.00000 0.00000 1.00000 0.00000
#> 
#> $keys
#> [1] "age"
#> 
#> $fun
#> function (x) 
#> {
#>     if (x %in% age_grade$age) {
#>         probs <- age_grade[which(age_grade$age == x), ][-1]
#>         out <- sample(names(age_grade)[-1], 1, prob = probs)
#>         out <- convert_grade(out)
#>         return(out)
#>     }
#>     else {
#>         return(NA)
#>     }
#> }
#> <environment: 0x000000001058a558>
```

Package Dependencies
--------------------

-   `dplyr`
-   `lubridate`
-   [wakefield](https://www.github.com/trinker/wakefield)
-   [simglm](https://www.github.com/lebebr01/simglm)

OpenSDP
-------

`OpenSDP.data` is part of the OpenSDP project.
