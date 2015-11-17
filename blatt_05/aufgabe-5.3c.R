#!/usr/bin/env Rscript

# P(a <= Y <= b) mit Y ~ Bin(n, p)
exakt <- function(a, b, n, p)
{
    p <- pbinom(b, n, p) - pbinom(a - 1, n, p)
    return (p)
}

# P(a <= Y <= b) mit Y ~ Bin(n, p) approximiert durch
# Normalverteilung
approx <- function(a, b, n, p)
{
    p <- pnorm((b + 0.5 - n * p) / sqrt(n*p*(1-p))) -
         pnorm((a - 0.5 - n * p) / sqrt(n*p*(1-p)))
    return (p)
}

cat("P(9 <= Y <= 11) mit Y ~ Bin(20, 0.5)\n")
cat("exakt:\n")
exakt(9, 11, 20, 0.5)

cat("approximativ:\n")
approx(9, 11, 20, 0.5)

cat('\n')

cat("P(90 <= Y <= 110) mit Y ~ Bin(200, 0.5)\n")
cat("exakt:\n")
exakt(90, 110, 200, 0.5)

cat("approximativ:\n")
approx(90, 110, 200, 0.5)

cat('\n')

cat("P(20 <= Y <= 40) mit Y ~ Bin(200, 0.15)\n")
cat("exakt:\n")
exakt(20, 40, 200, 0.15)

cat("approximativ:\n")
approx(20, 40, 200, 0.15)

cat('\n')

cat("P(0 <= Y <= 20) mit Y ~ Bin(200, 0.05)\n")
cat("exakt:\n")
exakt(0, 20, 200, 0.05)

cat("approximativ:\n")
approx(0, 20, 200, 0.05)
