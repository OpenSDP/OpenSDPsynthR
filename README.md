
<!-- README.md is generated from README.Rmd. Please edit that file -->
OpenSDPsynthR
=============

![](tools/figs/open_sdp_logo_red.png)

A project to generate realistic synthetic unit-level longitudinal education data to empower collaboration in education analytics.

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

To use `OpenSDPsynthR`, follow the instructions below:

Install Package
---------------

The development version of the package is able to be installed using the `install_github()`. To use this command you will need to install the `devtools` package.

``` r
devtools::install_github("opensdp/OpenSDPsynthR")
```

Make some data
--------------

Load the package

``` r
library(OpenSDPsynthR)
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
-   **Postsecondary institution:** name, city, state, online only, average net price, Pell grant rate, retention four year full time, share of part time enrollment, enrollment by race, SAT and ACT score distribution for admitted students

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

Cleaners
--------

You can reformat the synthetic data for use in specific types of projects. Currently two functions exist to format the simulated data into an analysis file matching the SDP College-going data specification and a CEDS-like data specification. More of these functions are planned in the future.

``` r
cgdata <- sdp_cleaner(out)
ceds <- ceds_cleaner(out)
```

Control Parameters
------------------

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
#> [27] "n_postsec"             "postsec_method"
```

These parameters can have complex structures to allow for conditional and random generation of data. Parameters fall into four categories:

-   **vectors:** a single list of parameters like school names, category names, or school IDs
-   **conditional probability list:** an R list that contains a variable to group by, a function to generate data with, and a list of parameters for that function for each group in the grouping variable
-   **outcome simulation parameters:** an R list of arguments to pass to the `simglm` function
-   **outcome adjustments:** an R list of lists, with functions that modify a variable in an existing data set

For more details, see the simulation control vignette.

``` r
vignette("Controlling the Data Simulation", package = "OpenSDPsynthR")
```

Package Dependencies
--------------------

-   `dplyr`
-   `lubridate`
-   [wakefield](https://www.github.com/trinker/wakefield)
-   [simglm](https://www.github.com/lebebr01/simglm)

OpenSDP
-------

`OpenSDPsynthR` is part of the OpenSDP project.

[OpenSDP](https://opensdp.github.io) is an online, public repository of analytic code, tools, and training intended to foster collaboration among education analysts and researchers in order to accelerate the improvement of our school systems. The community is hosted by the [Strategic Data Project](https://sdp.cepr.harvard.edu), an initiative of the [Center for Education Policy Research at Harvard University](https://cepr.harvard.edu). We welcome contributions and feedback.

These materials were originally authored by the Strategic Data Project.
