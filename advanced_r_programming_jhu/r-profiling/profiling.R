# Profiling in R
require(microbenchmark)
require(profvis)

microbenchmark(a <- rnorm(1000), 
               b <- mean(rnorm(1000)))

