#!/usr/bin/env Rscript

N          <- 10000
n          <- 20
mu         <- 10
sigma      <- 5
conf_level <- 0.9
matches    <- 0

for (i in 1:N) {
	x <- rnorm(n, mu, sigma)
	conf_interval <- t.test(
		x,conf.level = conf_level)$conf.int

	if (conf_interval[1] <= mu 
		&& mu <= conf_interval[2]) {
		
		matches <- matches + 1;
	}
}

cat("\nRel. Haeufigkeit, dass Erwartungswert ")
cat("im Konfidenzinterval:\n")
print(matches/N)

