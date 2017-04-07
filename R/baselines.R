

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
  } else if(bl == "grade"){
    data <- age_grade
    keys <- "age"
    fun <- function(x){
      if(x %in% age_grade$age){
        probs <- age_grade[which(age_grade$age == x),][-1]
        out <-sample(names(age_grade)[-1], 1, prob = probs)
        out <- convert_grade(out)
        return(out)
      } else{
        return(NA)
      }
    }
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
  if(baseline == "grade"){
    data <- as.data.frame(data)
    out <- sapply(data[, bl_data$keys], bl_data$fun)
    out <- as.character(out)
    data <- cbind(data, out)
    names(data)[ncol(data)] <- baseline
    data[,ncol(data)] <- as.character(data[,ncol(data)])
    return(data)
  }
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



#' Generate a random transition matrix for school enrollments
#'
#' @param nschls integer, the number of schools available, default = 15
#' @param diag_limit a numeric between 0 and 1 that sets the minimum probability that a student
#' will stay schools
#'
#' @return a transition matrix nschls X nschls of transition probabilities
#' @export
#'
#' @examples
#' out <- school_transitions(12)
#' out
school_transitions <- function(nschls = 15L, diag_limit = 0.9){
  stopifnot(class(nschls) %in% c("numeric", "integer"))
  stopifnot(diag_limit > 0 & diag_limit < 1)
  empty <- matrix(rep(0, nschls^2), nrow = nschls, ncol = nschls)
  diag(empty) <- 1
  for(i in 1:nschls){
    diag(empty)[i] <- runif(1, diag_limit, 1)
  }
  for(i in 1:nschls){
    empty[i, ][empty[i,] ==0] <- rand_vect_cont(nschls-1, 1-diag(empty)[i])
  }
  school_tm <- empty
  dimnames(school_tm) <- list(1:nschls, 1:nschls)
  return(school_tm)
}

#' Generate a grade advancement transition matrix
#'
#' @param ngrades integer, the number of grade levels to simulate
#' @param diag_limit the minimum probability of a student advancing to the next grade
#'
#' @return a matrix, ngrades x ngrades of grade transition probabilities
#' @export
grade_transitions <- function(ngrades=15L, diag_limit = 0.975){
  stopifnot(class(ngrades) %in% c("numeric", "integer"))
  stopifnot(diag_limit > 0 & diag_limit < 1)
  empty <- matrix(rep(0, ngrades^2), nrow = ngrades, ncol = ngrades)
  diag(empty) <- 1
  for(i in 1:ngrades){
    diag(empty)[i] <- runif(1, diag_limit, 1)
  }
  empty <- diag_offset(empty, 1L)
  for(g in 1:ngrades){
    i <- g + 1
    elem <- (i-2):(i+2)
    if(any(elem < 1) | any(elem > ngrades)){
      elem <- elem[elem >= 1]
      elem <- elem[elem <= ngrades]
    }
    elem <- elem[-which(elem == i)]
    if(i > ngrades){
      fill <- round(rand_vect_cont(N = length(elem), runif(1, diag_limit, 1),
                                   sd = 4), 4)
    } else{
      fill <- round(rand_vect_cont(N = length(elem), 1-empty[g, i], sd = 4), 4)
    }
    fill <- sort(fill, decreasing = TRUE)
    fill <- na.omit(fill[c(2, 1, 4, 3)])
    attributes(fill) <- NULL # strip NA info
    empty[g, ][elem] <- fill
  }
  empty[ngrades, ngrades] <- 1
  grades_tm <- empty
  dimms <- convert_grade(paste0("g", -1:(ngrades-2)))
  dimnames(grades_tm) <- list(dimms, dimms)
  grades_tm <- tm_convert(grades_tm)
  return(grades_tm)
}



#' Offset the diagonal values of a matrix
#'
#' @param matrix a square matrix
#' @param offset an integer value to offset the diagonal by
#'
#' @return a square matrix
#' @export
diag_offset <- function(matrix, offset = 1L){
  for(i in 1:nrow(matrix)){
    matrix[i, ] <- c(rep(0, offset), matrix[i, ][1:(length(matrix[i, ])-offset)])
  }
  return(matrix)
}




