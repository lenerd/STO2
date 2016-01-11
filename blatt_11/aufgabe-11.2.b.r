#!/usr/bin/env Rscript

hitOrMiss <- function(N) {

    x <- runif(N,0,1)
    y <- runif(N,0,1)
   
    hits <- 0

    for (i in 1:N) {
        expected <- sqrt(1-x[i]^2)
        if (y[i] <= expected) {
            hits <- hits + 1
        }
    }
    cat("\nErgebnisse fuer N =", N, "\n")
    cat("Punktschaetzung Pi/4:", hits/N, "\n")
    cat("Somit fuer Pi:", hits/N*4, "\n")
    cat("Konfidenzintervall:\n")
    binom.test(hits, N, 3.14159/4, conf.level=0.9)$conf.int
}

hitOrMiss(100)
hitOrMiss(1000)
