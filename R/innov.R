#' Simulates innovations
#'
#' @param nsim number of innovations.
#' @param dist a character string identifying the distribution.
#' @param ... parameters of the distribution.
#' @export
innov <-
    function(
        nsim,
        dist = c(
            "normal",
            "laplace",
            "digamma",
            "t",
            "stable"
        ),
        ...
    ) {
        dist <- match.arg(dist)
        fn <-
            switch(
                dist,
                "normal" = innov_normal,
                "laplace" = innov_laplace,
                "digamma" = innov_digamma,
                "t" = innov_t,
                "stable" = innov_stable
            )
        fn(nsim, ...)
    }

innov_rademacher <- function(nsim) sample(c(-1, 1), nsim, TRUE)

innov_normal <-
    function(nsim, mu = 0, sigma = 1) {
        stats::rnorm(nsim, mean = mu, sigma = sd)
    }

innov_t <- function(nsim, df) stats::rt(nsim, df)

innov_laplace <-
    function(nsim, lambda = 1 / sqrt(2)) {
        innov_rademacher(nsim) * stats::rexp(nsim, rate = 1 / lambda)
    }

innov_digamma <-
    function(nsim, alpha, beta) {
        innov_rademacher(nsim) * stats::rgamma(nsim, shape = alpha, rate = beta)
    }

innov_stable <-
    function(nsim, alpha) stabledist::rstable(nsim, alpha = alpha, beta = 0)