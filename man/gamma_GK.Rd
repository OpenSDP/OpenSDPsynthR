% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gamma_gk.R
\name{gamma_GK}
\alias{gamma_GK}
\title{Estimate the Goodman and Kruskal gamma statistic}
\usage{
gamma_GK(x, y = NULL, print = FALSE)
}
\arguments{
\item{x}{an unordered factor variable}

\item{y}{an unordered factor variable}

\item{print}{a logical vector indicating whether results should be printed to the console}
}
\value{
A named list with gamma, standard error of gamma, p-value of gamma, and statistical significance
}
\description{
Estimate the correlation between two unordered factor variables using the Goodman and Kruskal gamma statistic
}
\note{
Yadda yadda yadda
}
\references{
Adapted from Simon Jackman from: \url{http://jackman.stanford.edu/classes/151B/06/class0517.r}
}
\author{
Jared E. Knowles
}
