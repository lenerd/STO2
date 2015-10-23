#!/usr/bin/env Rscript

# transition probability matrix
# (transposed because R uses Fortran order)
P <- t(matrix(
              c(
                2/3, 0, 1/3,
                0, 0, 1,
                2/3, 1/3, 0
                ),
              nrow=3,
              ncol=3))

# startvector
alpha <- c(0.3, 0.1, 0.6)
#alpha <- c(1.0, 0.0, 0.0)
#alpha <- c(0.3, 0.4, 0.3)

cat("P =\n")
print(P)
cat("\U3b1 = ")
print(alpha)

pi_ <- alpha
for (i in 0:100)
{
    cat(sprintf("\U3C0^(%3d) = ", i))
    print(pi_)
    pi_ <- c(pi_ %*% P)
}
