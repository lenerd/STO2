#!/usr/bin/env Rscript

task <- function(mu_hypo_boundary) {

    N <- 10000
    n <- 30
    mu <- 50
    sigma <- 10

    sig_level <- 0.1
    err_type_1 <- 0
    err_type_2 <- 0

    is_h_0 <- mu >= mu_hypo_boundary
    is_h_1 <- !is_h_0

    for (i in 1:N) {
        x <- rnorm(n, mu, sigma)
        p <- t.test(x,
                    mu=mu_hypo_boundary,
                    alternative="less")$p.value

	is_rejected <- p < sig_level
	is_err_type_1 <- is_h_0 && is_rejected
	is_err_type_2 <- is_h_1 && !is_rejected

	if (is_err_type_1) err_type_1 <- err_type_1 + 1
	if (is_err_type_2) err_type_2 <- err_type_2 + 1
    }
    cat("\nErgebnisse fuer mu =", mu_hypo_boundary, "\n")
    cat("Anzahl Fehler Typ 1:", err_type_1, "\n")
    cat("Anazhl Fehler Typ 2:", err_type_2, "\n")
    cat("Ratio von Fehler Typ 1:", err_type_1/N, "\n")
    cat("Ratio von Fehler Typ 2:", err_type_2/N, "\n")
}

task(49)
task(51)

