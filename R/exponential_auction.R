##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Computation of the bidding function \code{beta} in case of
##' exponential density while using first price auction
##' @param x private value of the bidder
##' @param lambda rate of the exponential density
##' @param n number of bidders
##' @return optimal bidding value 
##' @author kaustav nandy
beta.exponential <- function(x, lambda = 1, n = 2)
{
    igrand <- function(x) (1 - exp(-(lambda * x)))^(n - 1)
    s <- integrate(igrand, lower = 0, upper = x)
    x - s$value / (1 - exp(-(lambda * x)))^(n - 1)
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Calculate bidding value for different private value in case
##' of exponential distribution
##' @param x.range private values for which bidding values are to be evaluated
##' @param lambda rate of the exponential distribution
##' @param n number of bidders
##' @return data frame with different bidding values corresponding to the 
##' @author kaustav nandy
beta.exponential.df <- function(x.range, lambda = 1, n = 2)
{
    beta <- sapply(x.range, beta.exponential, lambda = lambda, n = n)
    data.frame(beta = beta, x = x.range)
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title spline function to get inverse of bidding function in first
##' price auction
##' @param table.beta.exp data frame with two columns (named beta and
##' x) with bidding value and the private values respectively
##' @return 
##' @author kaustav nandy
calc.beta.inverse.exponential <- function(table.beta.exp)
{
    with(table.beta.exp, splinefun(x = beta, y = x))
}


## Test case
## library(lattice)
## lambda <- 0.5
## n <- 15
## x.range <- seq(0.01, 10, by = 0.05)
## out <- beta.exponential.df(x.range, lambda = lambda, n = n)
## with(out, xyplot(beta + x.range~x.range, type = "l"))

## with(out, xyplot(x.range~beta, type = "l"))
## beta.inv.sp <- calc.beta.inverse.exponential(seq(from = min(out$beta),
##                                                  to = max(out$beta),
##                                                  length.out = 200),
##                                              out)
## xyplot(x.range + beta.inv.sp ~ out$beta, type = "l")

## Theory: In second price auction, when my value is x, the bidder
## bids x, however she pays the second highest bid. So probability
## that the payment is >= R for some given R in (0, Inf) is
## $P(X_{(n - 1)} >= R)$.

## Distribution of $X_{(n - 1)}$
## In case of exponential,
##

calc.2nd.price.exponential <- function(R, n, lambda = 1)
{
    a <- exp(-lambda * R)
    1 - n * (1 - a)^(n - 1) + (n - 1) * (1 - a)^n
}

calc.1st.price.exponential <- function(R, n, lambda = 1, beta.inv.fun)
{
    1 - (1 - exp(-lambda * beta.inv.fun(R)))
}

n <- 5
lambda <- 2
R.min <- 0
R.max <- 4
R.len <- 100000
## Calculate x and beta(x) for different x so that we can use them for inverse
x.vals <- seq(R.min, R.max, length.out = R.len)
x.betax.df <- beta.exponential.df(x.vals, lambda = lambda, n = n)
beta.inv.fun <- calc.beta.inverse.exponential(x.betax.df)
R.vals <- seq(R.min, R.max, length.out = 1000)
out <-
    calc.1st.price.exponential(R.vals, n = n, lambda = lambda, beta.inv.fun = beta.inv.fun) -
        calc.2nd.price.exponential(R.vals, n = n, lambda = lambda)
xyplot(out, type = "l")
