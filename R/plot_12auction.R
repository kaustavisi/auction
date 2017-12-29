plot.1st.price <- function(R, n)
{
  1 - pmin(1, (n / (n - 1))^n * R^n)
}

plot.2nd.price <- function(R, n)
{
  1 - n * R^(n - 1) + (n-1) * R^n
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Calculates points from which 2nd price auction works better
##' than first price auction
##' @param n number of bidders
##' @param R.min minimum ..
##' @param R.max max of ..
##' @param R.len number of R values
##' @return Point at which 2nd price starts outdoing 1st price
##' @author kaustav nandy
calc.change.pt <- function(n, R.min = 0, R.max = 1, R.len = 1000)
{
    ## Choose R.len equidistant values between R.min and R.max
    R.range <- seq(R.min, R.max, length.out = R.len)
    ## evaluate plot.1st.price at all the points in R.range
    out.1st.price <- sapply(R.range, plot.1st.price, n = n)
    ## evaluate plot.2nd.price at all the points in R.range
    out.2nd.price <- sapply(R.range, plot.2nd.price, n = n)
    ## Find the point from where 2nd price auction is better than 1st
    ## price auction
    change.pt <- min(which(out.2nd.price > out.1st.price))
    change.pt * (R.max - R.min) / R.len + R.min
}


R.min <- 0
R.max <- 1
## TODO: There are warnings when one auction is strictly better that
## the other all the time. For moment, we need R in the whole
## resolution, so ignoring the warning for time being.
R.len <- 1000
n.range <- seq(2, 100, by = 1)
change.points <- sapply(n.range, calc.change.pt, R.min = R.min, R.max = R.max, R.len = R.len)
plot(change.points ~ n.range, xlab = "Number of Bidders", ylab = "R value", type = "l")
x11()

## Trying to fit a regression line just to find equation of the predicted line
library(lattice)
x <- n.range
foo <- lm(change.points ~ I(x / (x + 0.8)))
summary(foo)
fitted.val <- fitted(foo)
xyplot(fitted.val + change.points ~ n.range, type = "l")
