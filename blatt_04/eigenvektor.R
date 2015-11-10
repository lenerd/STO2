#!/usr/bin/env Rscript

# matrix G'
G_ <- matrix(
            c(
               1, 28, 28,  1,  1,  1,
              10, 10, 10, 10, 10, 10,
              19, 19,  1,  1,  1, 19,
               1,  1,  1,  1, 55,  1, 
               1,  1,  1, 28,  1, 28,
               1,  1,  1, 55,  1,  1
              ),
            nrow=6,
            ncol=6,
            byrow=TRUE) * 1/60

# left eigenvector
pi_ <- t(eigen(t(G_))$vec[,1])

print(pi_)
