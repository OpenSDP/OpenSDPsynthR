# Functions


assign_grade <- function(age, ability){
  baseGrade <- floor(age - 7)
  maxGrade <- floor(age - 4)
  out <- level(1, x = baseGrade:maxGrade, prob = c(0.01, 0.03, 0.9, 0.06))
  return(out)
}

# Expand DF into grid, from SO
#http://stackoverflow.com/questions/11693599/alternative-to-expand-grid-for-data-frames
expand.grid.df <- function(...) Reduce(function(...) merge(..., by=NULL), list(...))


# Function to generate conditional probabilities and append them to data
# Input is data frame, output is a data frame
# prob_list needs to have the levels of the factor variable and be the same length

# TODO: UNIT TESTS
# Document prob_list structure and verify
cond_prob <- function(data, factor, newvar, prob_list){
  data[, newvar] <- NA
  for(i in unique(data[, factor])){
    N <- nrow(data[data[,  factor] == i, ])
    data[data[,  factor] == i, newvar] <- do.call(prob_list[[i]]$f, 
                                                  c(list(n = N), 
                                                    prob_list[[i]]$pars))
    
  }
   return(data)
}

# From eeptools, and Jason Becker
age_calc <- function (dob, enddate = Sys.Date(), 
                      units = "months", precise = TRUE){
  if (!inherits(dob, "Date") | !inherits(enddate, "Date")) {
    stop("Both dob and enddate must be Date class objects")
  }
  if (any(enddate < dob)) {
    stop("End date must be a date after date of birth")
  }
  start <- as.POSIXlt(dob)
  end <- as.POSIXlt(enddate)
  if (precise) {
    start_is_leap <- ifelse(start$year%%400 == 0, TRUE, ifelse(start$year%%100 == 
                                                                 0, FALSE, ifelse(start$year%%4 == 0, TRUE, FALSE)))
    end_is_leap <- ifelse(end$year%%400 == 0, TRUE, ifelse(end$year%%100 == 
                                                             0, FALSE, ifelse(end$year%%4 == 0, TRUE, FALSE)))
  }
  if (units == "days") {
    result <- difftime(end, start, units = "days")
  }
  else if (units == "months") {
    months <- sapply(mapply(seq, as.POSIXct(start), as.POSIXct(end), 
                            by = "months", SIMPLIFY = FALSE), length) - 1
    if (precise) {
      month_length_end <- ifelse(end$mon == 1 & end_is_leap, 
                                 29, ifelse(end$mon == 1, 28, ifelse(end$mon %in% 
                                                                       c(3, 5, 8, 10), 30, 31)))
      month_length_prior <- ifelse((end$mon - 1) == 1 & 
                                     start_is_leap, 29, ifelse((end$mon - 1) == 1, 
                                                               28, ifelse((end$mon - 1) %in% c(3, 5, 8, 10), 
                                                                          30, 31)))
      month_frac <- ifelse(end$mday > start$mday, (end$mday - 
                                                     start$mday)/month_length_end, ifelse(end$mday < 
                                                                                            start$mday, (month_length_prior - start$mday)/month_length_prior + 
                                                                                            end$mday/month_length_end, 0))
      result <- months + month_frac
    }
    else {
      result <- months
    }
  }
  else if (units == "years") {
    years <- sapply(mapply(seq, as.POSIXct(start), as.POSIXct(end), 
                           by = "years", SIMPLIFY = FALSE), length) - 1
    if (precise) {
      start_length <- ifelse(start_is_leap, 366, 365)
      end_length <- ifelse(end_is_leap, 366, 365)
      start_day <- ifelse(start_is_leap & start$yday >= 
                            60, start$yday - 1, start$yday)
      end_day <- ifelse(end_is_leap & end$yday >= 60, end$yday - 
                          1, end$yday)
      year_frac <- ifelse(start_day < end_day, (end_day - 
                                                  start_day)/end_length, ifelse(start_day > end_day, 
                                                                                (start_length - start_day)/start_length + end_day/end_length, 
                                                                                0))
      result <- years + year_frac
    }
    else {
      result <- years
    }
  }
  else {
    stop("Unrecognized units. Please choose years, months, or days.")
  }
  return(result)
}
# height_list <- list("Male" = list(f = height_in, pars = list(mean = 70, sd = 3.8)), 
#                     "Female" = list(f = height_in, pars = list(mean = 64, sd = 3.2)))

# Function to load baseline data
get_baseline <- function(bl){
  if(bl == "ell"){
    readRDS("data/ell.rds")
  }
}

# TODO:
# Structure baseline
# Make it a list
# Make the list include the key variables needed
# Make the list include the levels of factors to match on 


# x and y are vectors of labels
reconcile_labels <- function(x, y){
  list(x, y[pmatch(tolower(x), tolower(y))])
}

try_label <- function(x, y, value){
  lookup <- reconcile_labels(x, y)
  if(is.na(y[value])){
    out <- lookup[[2]][which(lookup[[1]] == value)]
    if(length(out) != 0){
      return(out)
    } else{
      return(NA)
    }
  } else{
    return(y[value])
  }
}

assign_baseline <- function(baseline = NULL, data, key){
  bl_data <- get_baseline(baseline)
  
  
  
  gen_fun <- function(x, y){
    tmp_race <- as.character(x)
    tmp_age <- y
    idx <- which(tolower(names(bl_data)) == tolower(tmp_race))
    prob <- bl_data[bl_data$age == tmp_age, idx]
    out <- rbinom(1, 1, prob = prob)
    return(out)
  }
  
  out <- mapply(gen_fun, x = stu_year[,"Race"], y = stu_year[,"age_int"])
  
  out <- lapply(stu_year, function(x) gen_fun(x))
  
  tmp_race <- as.character(demog_master$Race[1])
  tmp_age <- round(stu_year$age[1], 0)
  
  
  
  
  
}

