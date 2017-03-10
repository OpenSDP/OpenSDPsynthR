# Transition matrix series switching 
#http://stats.stackexchange.com/questions/14175/how-to-generate-random-auto-correlated-binary-time-series-data


# n = length of the series to create
# TransitionMatrix is a 2x2 matrix where 
# Element 1, 1 is the probability of moving to state 0 conditional on being in state 0
# Element 2, 1 is the probability of moving to state 0 conditional on being in state 1
# Element 1, 2 is the probability of moving to state 1 conditional on being in state 0
# ELement 2, 2 is the probability of moving to state 1 conditional on being in state 1
# Rows represent the probability choices within a state
# https://en.wikipedia.org/wiki/Examples_of_Markov_chains
createSeries <- function(n, TransitionMatrix){
  stopifnot(is.matrix(TransitionMatrix))
  stopifnot(n>0)
  
  Series <- c(1, rep(NA,n-1))
  random <- runif(n-1)
  for (i in 2:length(Series)){
    Series[i] <- TransitionMatrix[Series[i-1]+1, 1] >= random[i-1]
  }
  
  return(Series)
}

createSeries(10, matrix(c(0.6,0.4,0.9,0.1), nrow=2, byrow=TRUE))

createAutocorBinSeries = function(n=100,mean=0.5, corr=0){ 
  p01=corr*(1-mean)/mean 
  createSeries(n,matrix(c(1-p01,p01,corr,1-corr),nrow=2,byrow=T)) 
  }

createAutocorBinSeries(n=12,mean=0.5,corr=0.9)
createAutocorBinSeries(n=100,mean=0.5,corr=0.1)


findTransitions <- function(series, return = c("matrix", "simple")){
  if(missing(return)){
    return <- "matrix"
  }
  # TODO check to ensure this coerces into a true transition matrix
  tab <- table(paste0(head(series,-1),tail(series,-1))) 
  tab <- matrix(c(tab["00"], tab["01"], tab["10"], tab["11"]), 
                ncol = 2, byrow =TRUE)
  tab[is.na(tab)] <- 0
  tab <- prop.table(tab, 1)
  tab[is.na(tab)] <- 0
  if(return == "matrix"){
    tab
  } else {
    # TODO: Check
    # Is this the right math?
    p01 <- tab[1, 2]
    corr <- tab[2, 2]
    mean <- p01/corr - 1
    return(list(mean = mean, corr = corr))
  }
}

# createSeries(10, matrix(c(0.444, 0.111, 0.222, 0.222), nrow = 2, byrow =TRUE))
# createSeries(10, findTransitions(series))
# outList <- replicate(25, createAutocorBinSeries(n=10,mean=0.4,corr=0.8), 
#                      simplify = "array")
# 
# outList <- apply(outList, 2, function(x) table(paste0(head(x, -1), tail(x,-1))))
# 
# map(outList, function(x) sum(x[names(x) == "00"])) %>% reduce_right(sum)
# map(outList, function(x) sum(x[names(x) == "10"])) %>% reduce_right(sum)
# map(outList, function(x) sum(x[names(x) == "01"])) %>% reduce_right(sum)
# map(outList, function(x) sum(x[names(x) == "11"])) %>% reduce_right(sum)
# 
# apply(outList, 2, findTransitions)

# 
# out <- findTransitions(series)
# 
# p01=corr*(1-mean)/mean 
# createSeries(n,matrix(c(1-p01,p01,corr,1-corr),nrow=2,byrow=T))
# 
# series <- createAutocorBinSeries(n=5000, mean=0.1, corr=0.8)
# out <- findTransitions(series)
# out <- findTransitions(series, return = "simple")
# 
#              
# mapply(function(x, y) (x*(1-y) / y), seq(0, 1, 0.1), seq(0, 1, 0.1))
# mapply(function(x, y) (x*(1-y) / y), seq(1, 0, -0.1), seq(1, 0, -0.1))
# mapply(function(x, y) (x*(1-y) / y), seq(0, 1, 0.1), seq(1, 0, -0.1))
# mapply(function(x, y) (x*(1-y) / y), seq(1, 0, -0.1), seq(0, 1, 0.1))
# p01=corr*(1-mean)/mean 
# 
# testdf <- expand.grid(mean = seq(0.01, 0.99, 0.1), corr = seq(0.01, 0.99, 0.1))
# testdf$p01 <- mapply(function(x, y) (x*(1-y) / y), testdf$corr, testdf$mean)
# 
# createSeries(100 , matrix(c(1-90, 90, 0.91, .09),nrow=2,byrow=T)) 
