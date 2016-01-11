#!/usr/bin/env Rscript

crude <- function(N) {


    x <- runif(N, 0, 1)
    y <- sqrt(1-x^2)
    T <- sum(y) / N


    cat("\nErgebnisse fuer N =", N, "\n")
    cat("Punktschaetzung Pi/4:", T, "\n")
    cat("Somit fuer Pi:", T*4, "\n")
    cat("Konfidenzintervall:\n")
    t.test(y, conf.level=0.9)$conf.int
}

crude(100)
crude(1000)
crude(100000)
