##
##  f s o l v e . R  test suite
##

fsolve <- pracma::fsolve

## Solve a small square system with an analytic Jacobian.
## Previously the analytic-Jacobian wrapper `J <- function(x) J(x, ...)`
## self-recursed instead of calling the saved `Jun`, causing infinite
## recursion. The correct wrapper pattern is the same as in gaussNewton().
F <- function(x) c(x[1]^2 + x[2]^2 - 1, x[1] - x[2])
J <- function(x) matrix(c(2*x[1], 1, 2*x[2], -1), 2, 2)
sol <- fsolve(F, c(1, 1), J = J)

## Solution lies on x1 = x2 with x1^2 + x2^2 = 1, i.e. (1/sqrt(2), 1/sqrt(2)).
stopifnot(all.equal(sol$x, c(1/sqrt(2), 1/sqrt(2)), tolerance = 1e-6))
stopifnot(max(abs(sol$fval)) < 1e-6)

## Sanity: the numerical-Jacobian path still works and finds the same root.
sol0 <- fsolve(F, c(1, 1))
all.equal(sol0$x, sol$x, tolerance = 1e-6)

## E o F
