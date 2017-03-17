
# make_markov_series

context("Test Markov Generator")

test_that("Markov Chain works", {
  statesNames <- c("No", "Yes")
  tm <- matrix(c(800, 10, 200, 200), nrow = 2, byrow = TRUE,
               dimnames = list(statesNames, statesNames))
  expect_warning(make_markov_series(10, tm = tm))
  expect_error(make_markov_series(0, tm = tm))
  tm <- tm / rowSums(tm)
  expect_equal(length(make_markov_series(10, tm = tm)), 10)
  expect_equal(length(make_markov_series(10, tm = tm, t0 = "Yes")), 10)
  expect_equal(length(make_markov_series(10, tm = tm, t0 = "Yes",
                                         include.t0 = TRUE)), 11)
  expect_equal(length(make_markov_series(10, tm = tm, t0 = "No",
                                         include.t0 = TRUE)), 11)
  expect_true(all(make_markov_series(10, tm = tm,
                                     t0 = "Yes", include.t0 = TRUE) %in% statesNames))
})


# Test accuracy

set.seed(458397)

test_fun <- function(basetm, runLength, ...){
  zzz <- make_markov_series(runLength, tm = basetm)
  out <- fit_series(zzz, return = "fit", ...)
  return(all(c(all(out$lowerEndpointMatrix < basetm),
               all(basetm < out$upperEndpointMatrix))))
}

test_that("Markov series are correct", {
  dumbTM <- structure(c(0.5, 0.5, 0.5, 0.5), .Dim = c(2L, 2L), .Dimnames = list(
    c("No", "Yes"), c("No", "Yes")))
  zzz <- replicate(500, test_fun(basetm = dumbTM, runLength = 75))
  per_true <- length(zzz[zzz == TRUE]) / length(zzz)
  expect_true(0.87 < per_true)
  expect_true(0.92 > per_true)
  statesNames <- c("No", "Yes")
  dumbTM2 <-  matrix(c(0.5, 0.5, 0.5, 0.5), nrow = 2, byrow = TRUE,
                     dimnames = list(statesNames, statesNames))
  zzy <-  replicate(500, test_fun(basetm = dumbTM2, runLength = 10,
                        possibleStates = statesNames))
  per_true2 <- length(zzy[zzy == TRUE]) / length(zzy)
  expect_true(per_true2 < per_true)
})

test_that("fit series respects user input", {
  dumbTM <- structure(c(0.5, 0.5, 0.5, 0.5), .Dim = c(2L, 2L), .Dimnames = list(
    c("No", "Yes"), c("No", "Yes")))
  zzz <- make_markov_series(20, tm = dumbTM)
  out <- fit_series(zzz, return = "fit")
  expect_is(out$estimate, "markovchain")
  expect_is(out, "list")
  expect_equal(out$confidenceLevel, 0.95)
  out <- fit_series(zzz, return = "fit", confidencelevel = 0.9)
  expect_equal(out$confidenceLevel, 0.9)
  out <- fit_series(zzz, return = "matrix")
  expect_is(out, "markovchain")
})

# fit_series

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
