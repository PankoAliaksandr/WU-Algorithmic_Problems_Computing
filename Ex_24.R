# Task 24
# Write a function to compute

n <- c(50,100,150,200,250)
a <- lgamma((n-1)/2)
b <- lgamma(1/2)
c <- lgamma((n-2)/2)

orig_a <- exp(a)
orig_b <- exp(b)
orig_c <- exp(c)
ro <-orig_a / (orig_b * orig_c)
 
cat("ro =",ro)

lim <- ro/sqrt(n)
cat(lim)
cat("limit = 1/ sqrt(pi)")


