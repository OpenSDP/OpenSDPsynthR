##' Estimate the Goodman and Kruskal gamma statistic
##'
##' Estimate the correlation between two unordered factor variables using the Goodman and Kruskal gamma statistic
##'
##' @param x an unordered factor variable
##' @param y an unordered factor variable
##' @param print a logical vector indicating whether results should be printed to the console
##' @return A named list with gamma, standard error of gamma, p-value of gamma, and statistical significance
##' @note Yadda yadda yadda
##' @export
##' @author Jared E. Knowles
##' @references Adapted from Simon Jackman from: \url{http://jackman.stanford.edu/classes/151B/06/class0517.r}
gamma_GK <- function(x, y = NULL, print = FALSE){
  concordant <- function(x){
    ## get sum(matrix values > r AND > c)
    ## for each matrix[r, c]
    mat.lr <- function(r,c){
      lr <- x[(r.x > r) & (c.x > c)]
      sum(lr)
    }

    ## get row and column index for each
    ## matrix element
    r.x <- row(x)
    c.x <- col(x)

    ## return the sum of each matrix[r, c] * sums
    ## using mapply to sequence thru each matrix[r, c]
    sum(x * mapply(mat.lr, r = r.x, c = c.x))
  }

  discordant <- function(x){
    ## get sum(matrix values > r AND < c)
    ## for each matrix[r, c]
    mat.ll <- function(r,c){
      ll <- x[(r.x > r) & (c.x < c)]
      sum(ll)
    }

    ## get row and column index for each
    ## matrix element
    r.x <- row(x)
    c.x <- col(x)

    ## return the sum of each matrix[r, c] * sums
    ## using mapply to sequence thru each matrix[r, c]
    sum(x * mapply(mat.ll, r = r.x, c = c.x))
  }

  if(is.table(x) | is.matrix(x)){
    c <- concordant(x)
    d <- discordant(x)
    n <- sum(x)
  }
  else{
    tab <- table(x,y)
    c <- concordant(tab)
    d <- discordant(tab)
    n <- sum(tab)
  }
  gamma <- (c - d) / (c + d)

  arg <- (c+d)/(n*(1-(gamma^2)))
  stdError <- 1/sqrt(arg)
  z <- gamma/stdError
  if(print==TRUE){
    cat("Goodman-Kruskal gamma statistic:\n")
    cat(paste("Concordant Pairs",c,"\n"))
    cat(paste("Discordant Pairs",d,"\n\n"))
    cat(paste("Estimate of gamma:",
              signif(gamma,.Options$digits),
              "Standard error:",
              signif(stdError,.Options$digits),
              "\n\n"))

    cat(paste("H0: gamma = 0 vs HA: two-sided\n"))
    cat(paste("z:",
              signif(z, .Options$digits),
              "p-value:",
              signif(2*(1-pnorm(abs(z))), .Options$digits),
              "\n\n"))
    if(c<51 | d<51){
      cat("Warning: p-values are based on a normal approximation to the\n")
      cat("sampling distribution of the z test statistic, which is commonly\n")
      cat("considered to be good only if C and D are both > 50.\n")
    }
  }
  return(list(gamma = signif(gamma,.Options$digits),
              se = signif(stdError,.Options$digits),
              z =signif(z, .Options$digits),
              sig = signif(2*(1-pnorm(abs(z))), .Options$digits)))
  invisible(NULL)
}
