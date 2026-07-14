##
##  q u a d g k . R  test suite
##

quadgk <- pracma::quadgk

## Documented smooth cases still integrate correctly.
all.equal(quadgk(function(x) exp(x)*sin(x), 0, pi), 12.0703463163896,
          tolerance = 1e-13)
all.equal(quadgk(function(t) log(1-t)/t, 1, 0, tol = 1e-12),
          pi^2/6, tolerance = 1e-10)

## Regression: when the adaptive recursion reaches the minimum step size
## (here forced by a high-amplitude, oscillatory integrand whose absolute
## error estimate never drops below `tol`), quadgk must return the last
## computed Gauss-Kronrod estimate `Q15` -- not an unbound variable `Q2`.
g <- function(x) 1e290 * sin(50*x)
Q <- suppressWarnings(quadgk(g, 0, 1))
stopifnot(is.numeric(Q), is.finite(Q))

## E o F
