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
  if(!factor %in% names(demog_master)){
    stop("Factor not found in data. Did you forget to create it?")
  }
  data[, newvar] <- NA
  # Error checking, if not all factors are defined in prob_list, issue error
  if(!all(unique(data[, factor]) %in% names(prob_list))){
    missingLevels <- 
      as.character(unique(data[, factor])[!unique(data[, factor]) %in% 
                                            names(prob_list)])
    msg <- paste0("Probability list does not specify all possible factor levels.", 
                  "\n", "Missing levels: \n", 
                  paste0(missingLevels, collapse = ",\n"))
    stop(msg)
  }
  # If not all prob_list elements are listed in the data, issue a warning
  if(!all(names(prob_list) %in% unique(data[, factor]))){
    missingLevels <- 
      as.character(names(prob_list)[!names(prob_list) %in% 
                                      unique(data[, factor])])
    msg <- paste0("Probability list elements not found in data.", 
                  "\n", "Missing levels: \n", 
                  paste0(missingLevels, collapse = ",\n"))
    warning(msg)
  } 
   
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
    data <- readRDS("data/ell.rds")
    keys <- c("race", "age")
    fun <- function(x) rbinom(1, 1, x)
  }
  return(list(data = data, keys = keys, fun = fun))
}


assign_baseline <- function(baseline = NULL, data){
  bl_data <- get_baseline(baseline)
  data <- as.data.frame(left_join(data, bl_data$data, by = bl_data$keys))
  var <- names(bl_data$data)[!names(bl_data$data) %in% bl_data$keys]
  if(length(var) > 1){
    stop("Variables are labeled wrong in data.")
  }
  out <- sapply(data[, var], bl_data$fun)
  return(out)
}


varMap <- function(local, category = NULL, CEDS = NULL){
  CEDS_map <- readRDS("data/sdp_ceds_map.rds")
  out <- CEDS_map$sdp_name[match(local, CEDS_map$CEDS_name)]
  if(all(is.na(out))){
    out <- NULL
  } else{
    return(out)  
  }
}

# Convert xwalk format into labels and levels for recoding
get_code_values <- function(x){
  tmp <- strsplit(x, split = ";")
  tmp <- sapply(tmp, strsplit, split = "[[:punct:]]")
  levels <- map(tmp, 1) %>% unlist
  labels <- map(tmp, 2) %>% unlist
  labels <- trimws(labels, which = )
  return(list(levels = levels, labels = labels))
}

# TODO: This should always append
#http://stackoverflow.com/questions/35943455/creating-indicator-variable-columns-in-dplyr-chain
make_inds <- function(data, col) {
  for(i in col) {
    idx <- which(names(data)==i)
    v <- data[[idx]]
    stopifnot(class(v)=="factor")
    m <- matrix(0, nrow=nrow(data), ncol=nlevels(v))
    m[cbind(seq_along(v), as.integer(v))]<-1
    colnames(m) <- paste(levels(v))
    r <- data.frame(m)
    # Only need this if you want to drop original column
    # if (idx > 1) {
    #   r <- cbind(data[1:(idx-1)],r)
    # }
    # if (idx < ncol(data)) {
    #   r <- cbind(r, data[(idx+1):ncol(data)])
    # }
    data <- cbind(data, r)
  }
  data
}

# TODO:
# Structure baseline
# Make it a list
# Make the list include the key variables needed
# Make the list include the levels of factors to match on 


# # x and y are vectors of labels
# reconcile_labels <- function(x, y){
#   list(x, y[pmatch(tolower(x), tolower(y))])
# }
# 
# try_label <- function(x, y, value){
#   lookup <- reconcile_labels(x, y)
#   if(is.na(y[value])){
#     out <- lookup[[2]][which(lookup[[1]] == value)]
#     if(length(out) != 0){
#       return(out)
#     } else{
#       return(NA)
#     }
#   } else{
#     return(y[value])
#   }
# }

