#!/usr/bin/env Rscript

# Taken from http://www.stat.cmu.edu/~cshalizi/402/lectures/08-bootstrap/pareto.R
# at 2016-01-18
# ~~~
# Quantiles of Pareto distributions
# Input: vector of probabilities, lower threshold, scaling exponent, usual flags
# Output: Vector of quantile values
qpareto <- function(p, threshold=1, exponent, lower.tail=TRUE, log.p=FALSE) {
  # Quantile function for Pareto distribution
  # P(x) = 1 - (x/xmin)^(1-a)
  # 1-p = (x(p)/xmin)^(1-a)
  # (1-p)^(1/(1-a)) = x(p)/xmin
  # xmin*((1-p)^(1/(1-a))) = x(p)
  # Upper quantile:
  # U(x) = (x/xmin)^(1-a)
  # u^(1/(1-a)) = x/xmin
  # xmin * u^(1/(1-a)) = x
  # log(xmin) + (1/(1-a)) log(u) = log(x)
  if (log.p) {
    p <- exp(p)
  }
  if (lower.tail) {
    p <- 1-p
  }
  # This works, via the recycling rule
  # q<-(p^(1/(1-exponent)))*threshold
  q.log <- log(threshold) + (1/(1-exponent))*log(p)
  q <- exp(q.log)
  return(q)
}

# Taken from http://www.stat.cmu.edu/~cshalizi/402/lectures/08-bootstrap/pareto.R
# at 2016-01-18
# ~~~
# Generate Pareto-distributed random variates
# Input: Integer size, lower threshold, scaling exponent
# Output: Vector of real-valued random variates
rpareto <- function(n, threshold=1, exponent) {
  # Using the transformation method, because we know the quantile function
  # analytically
  # Consider replacing with a non-R implementation of transformation method
  ru <- runif(n)
  r<-qpareto(ru,threshold,exponent)
  return(r)
}


analyzeConfInt <- function(N, threshold, exponent) {
    x <- rpareto(N,threshold,exponent)
    #print(x)
    confInt <- t.test(x[1:2], conf.level=0.9)$conf.int
    confIntWidths <- c() 
    sizes <- c()

    for (i in 2:N) {
        confInt = t.test(x[1:i], conf.level=0.9)$conf.int
        confIntWidths[i] <- confInt[2] - confInt[1]
        sizes[i] <- i        

        if (i %% 100 == 0) {
            confIntWidths[i]
        }
    }
    
    plot(x=sizes,y=confIntWidths)
}

cat("N=1000;x0=1;k=0.5\n")
analyzeConfInt(1000,1,0.5)

cat("N=1000;x0=1;k=1.5\n")
analyzeConfInt(1000,1,1.5)

cat("N=1000;x0=1;k=2.5\n")
analyzeConfInt(1000,1,2.5)

