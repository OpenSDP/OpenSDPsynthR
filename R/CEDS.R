#' Map to and from CEDS column names
#'
#' @param user character vector of variable names to match
#' @param category character, a category of CEDS data to match
#' @param CEDS optional
#'
#' @return mapped CEDS names
#' @export
map_CEDS <- function(user, category = NULL, CEDS = NULL){
  if(!all(class(user) %in% c("factor", "character"))){ # is all right conditional?
    msg <- "Please supply a character vector of names to be converted. Consider
    passing or reassigning names(object)"
    stop(msg)
  }
  # TODO: Create a crosswalk from CEDS to SDP and SDP to CEDS with clearer lookup
  sdp <- xwalk$sdp_name[match(user, xwalk$CEDS_name)]
  ceds <- xwalk$CEDS_name[match(user, xwalk$sdp_name)]
  checkLength <- function(x, y){
    x_l <- length(x[!is.na(x)])
    y_l <- length(y[!is.na(y)])
    if(x_l > 0 & y_l > 0){
      msg <- ("Both SDP and CEDS names detected.")
    } else{
      msg <- " " # empty space to allow compound messages below
    }
    if(x_l > y_l){
      message("Majority of names match SDP names, returning SDP names. ", msg)
      return(x)
    } else if(y_l > x_l){
      message("Majority of names match CEDS names, returning CEDS names. ", msg)
      return(y)
    } else {
      stop("Equal number of CEDS and SDP names identified, check names.")
    }
  }
  out <- checkLength(sdp, ceds)
  if(any(is.na(out))){
    message("Not all names successfully matched. Returning NA for those not matched.")
  }
  if(all(is.na(out))){
    warning("No names could be matched, please check names for typos.")
    out <- NULL
  } else{
    return(out)
  }
}

#' Get values of codes from the CEDS Crosswalk List
#'
#' @param x the data.frame character element that contains the codes
#' @importFrom purrr map
#' @importFrom magrittr %>%
#' @export
#' @return a list with the labels and levels properly formatted
get_code_values <- function(x){
  tmp <- unlist(strsplit(x, split = ";"))
  tmp <- sapply(tmp, strsplit, split = "[[:punct:]]")
  values <- map(tmp, 1) %>% unlist
  labels <- map(tmp, 2) %>% unlist
  labels <- trimws(labels)
  names(labels) <- NULL
  values <- trimws(values)
  names(values) <- NULL
  return(list(values = values, labels = labels))
}

# TODO: This should always append
#' Append indicator variables to a data frame based on a single factor variable
#'
#' @param data a data frame
#' @param col character, name of the factor column to generate indicators for
#' @source \url{http://stackoverflow.com/questions/35943455/creating-indicator-variable-columns-in-dplyr-chain}
#' @return a data frame with the factor levels appended as columns
#' @export
make_inds <- function(data, col) {
  for(i in col) {
    idx <- which(names(data)==i)
    v <- data[[idx]]
    if(class(v) != "factor"){
      warning("Factor not supplied, coercing...")
      v <- factor(v)
    }
    # stopifnot(class(v)=="factor")
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

#' Recode options
#'
#' @param data a data.frame to recode the variables to CEDS from
#' @param from the data definitions you are recoding from
#'
#' @return the data object, but with all values matching CEDS specification
#' recoded to meet the CEDS specification
#' @export
recode_options <- function(data, from = c("SDP", "CEDS")){
  if(missing(from)){
    from <- "CEDS"
  }
  stopifnot(all(names(data) %in% xwalk$CEDS_name) |
              all(names(data) %in% xwalk$sdp_name))
  recode_ceds_value <- function(var, options){
    var <- options$labels[match(var, options$values)]
    return(var)
  }
  for(i in names(data)){
    if(from == "SDP"){
      codes <- get_code_values(xwalk$SDP_option_match[xwalk$sdp_name == i])
    } else{
      CEDS <- xwalk$schema[xwalk$CEDS_name == i][[1]]
    }
    data[, i] <- recode_ceds_value(data[, i], codes)
  }
  return(data)
}

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
