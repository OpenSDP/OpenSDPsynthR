
# make_markov_series
# fit_series



test_string <- make_binary_series(1000, mean = 0.25, corr = 0)

mean(test_string)


trans.matrix <- function(X, prob=T)
{
  tt <- table( c(X[,-ncol(X)]), c(X[,-1]) )
  if(prob) tt <- tt / rowSums(tt)
  tt
}


X <- c(1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 0, 0, 1, 1, 1, 1)



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
