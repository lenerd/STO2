#!/usr/bin/env Rscript

# transition probability matrix
# (transposed because R uses Fortran order)
P <- t(matrix(
            c(1/2, 1/2, 0, 0, 0, 0, 0, 0,
              0, 1, 0, 0, 0, 0, 0, 0,
              0, 0, 1, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 1/2, 1/2,
              1/2, 1/2, 0, 0, 0, 0, 0, 0,
              0, 0, 1/2, 1/2, 0, 0, 0, 0,
              0, 0, 0, 0, 1/2, 1/2, 0, 0,
              0, 0, 0, 0, 0, 0, 1/2, 1/2),
            nrow=8,
            ncol=8))

# startvector
alpha <- c(
           0.125,
           0.125,
           0.125,
           0.125,
           0.125,
           0.125,
           0.125,
           0.125
          )

cat("P =\n")
print(P)
cat("\U3b1 =")
print(alpha)

pi_ <- alpha
for (i in 1:100)
    pi_ <- c(pi_ %*% P)

cat("\U3C0^(100) = ")
print(pi_)
