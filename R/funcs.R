# Functions

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


# height_list <- list("Male" = list(f = height_in, pars = list(mean = 70, sd = 3.8)), 
#                     "Female" = list(f = height_in, pars = list(mean = 64, sd = 3.2)))

