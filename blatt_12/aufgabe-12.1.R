#!/usr/bin/env Rscript

x0 <- 1
k1 <- 0.5
k2 <- 1.5
k3 <- 2.5
N <- 1000
u <- runif(N)

pseudoinverse <- function (y, k)
{
    return (x0 / (1-y)^(1/k))
}

analyze <- function(k)
{
    p <- pseudoinverse(u, k)
    cw <- numeric(N)
    for (i in 2:N)
    {
        res <- t.test(p[1:i], conf.level=0.9)
        cw[i] <- res$conf.int[2] - res$conf.int[1]
    }
    plot(cw, main=sprintf("k = %.1f", k))
}

analyze(k1)
analyze(k2)
analyze(k3)
