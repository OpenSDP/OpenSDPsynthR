

#' Function to load in SDP default baseline data
#'
#' @param bl a character naming the type of baseline available
#'
#' @return the restored baseline object
#' @export
get_baseline <- function(bl){
  if(bl == "ell"){
    #data(sysdata, envir=environment())
    data <- ell
    keys <- c("race", "age")
    fun <- function(x) rbinom(1, 1, x)
  } else if(bl == "ses"){
    data <- ses
    keys <- c("race")
    fun <- function(x) rbinom(1, 1, x)
  } else if(bl == "program"){
    data <- prog_baseline
    keys <- NULL
    fun <- function() {prog_baseline[sample(rownames(prog_baseline), 1,
                                            prob = prog_baseline$prob), 1:3]}

  } else{
    stop("Baseline not currently defined. Maybe you can write your own?")
  }
  return(list(data = data, keys = keys, fun = fun))
}

#' Append baseline data to initial data
#'
#' @param baseline character value of the default baseline to assign
#' @param data a data.frame to append the baseline to
#'
#' @return the data.frame passed by the user with an additional variable appended
#' @importFrom dplyr left_join
#' @export
assign_baseline <- function(baseline = NULL, data){
  bl_data <- get_baseline(baseline)
  if(is.null(bl_data$keys)){
    out <- replicate(nrow(data), bl_data$fun(), simplify=FALSE) %>%
      Reduce("rbind", .)
    out <- cbind(data, out)
  } else {
    if(any(!bl_data$keys %in% names(data))){
      msg <- paste("Data supplied does not have right keys to merge. Please use columns:",
                   paste(bl_data$keys, collapse = ", "), sep = " \n ")
      stop(msg)
    }
    data <- as.data.frame(left_join(data, bl_data$data, by = bl_data$keys))
    var <- names(bl_data$data)[!names(bl_data$data) %in% bl_data$keys]
    if(length(var) > 1){
      stop("Variables are labeled wrong in data.")
    }
    # Avoid binomial NA warning in ELL baseline
    suppressWarnings({
      out <- sapply(data[, var], bl_data$fun)
    })
  }
  return(out)
}

# Function to generate conditional probabilities and append them to data
# Input is data frame, output is a data frame
# prob_list needs to have the levels of the factor variable and be the same length

# TODO: UNIT TESTS
# Document prob_list structure and verify

#' Generate conditional probabilities by group
#'
#' @param data dataframe to add variable to
#' @param factor grouping variable that probability of \code{newvar} is conditional on
#' @param newvar name, character, of new variable defined by \code{prob_list}
#' @param prob_list a list, defining the way \code{newvar} should be generated
#'
#' @return data.frame with \code{newvar} appended to dataframe
#' @export
cond_prob <- function(data, factor, newvar, prob_list){
  # Work around tbls
  if(any(class(data) != "data.frame")){
    data <- as.data.frame(data)
  }
  if(!factor %in% names(data)){
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

#' Assign student a grade
#'
#' @param age age of the student in years
#' @param ability a modifier that signifies student ability?
#'
#' @return a vector of grade levels
#' @importFrom wakefield level
#' @export
#'
#' @examples
#' age <- c(9, 10, 11, 12)
#' assign_grade(age = age)
assign_grade <- function(age, ability){
  baseGrade <- floor(age - 7)
  maxGrade <- floor(age - 4)
  out <- wakefield::level(1, x = baseGrade:maxGrade, prob = c(0.01, 0.03, 0.9, 0.06))
  return(out)
}

