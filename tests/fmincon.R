##
##  f m i n c o n . R  test suite
##

fmincon <- pracma::fmincon

## 'fmincon' depends on the suggested packages 'NlcOptim' and 'quadprog'.
if (requireNamespace("NlcOptim", quietly = TRUE) &&
    requireNamespace("quadprog", quietly = TRUE)) {

    fn <- function(x) sum((x - c(1, 2, 3))^2)
    x0 <- c(0, 0, 0)

    ## A length-1 bound is recycled to length(x0) and the optimum is found.
    sol <- fmincon(x0, fn, lb = 1)
    all.equal(sol$par, c(1, 2, 3), tolerance = 1e-4)
    stopifnot(all(sol$par >= 1 - 1e-6))

    ## Regression: bounds whose length is neither 1 nor length(x0) must be
    ## rejected. Previously `length(lb == 1)` always equalled `length(lb)`,
    ## silently bypassing the length check and recycling wrong-length bounds.
    err_lb <- tryCatch(fmincon(x0, fn, lb = c(0, 0)),
                       error = function(e) conditionMessage(e))
    stopifnot(grepl("Length of argument 'lb' must be equal to length\\(x0\\)",
                    err_lb))

    err_ub <- tryCatch(fmincon(x0, fn, ub = c(Inf, Inf)),
                       error = function(e) conditionMessage(e))
    stopifnot(grepl("Length of argument 'ub' must be equal to length\\(x0\\)",
                    err_ub))
}

## E o F
