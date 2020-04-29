# Task 5
# Calculate the S = sum(1/i) for i=[1:n]
# compare it to log(n) for n = 500,1000,2000,4000,8000.
# Use the quick formula to compute the sum for all values of n between 1
# and 100, and store the results in a vector.

check <- c(500,1000,2000,4000,8000)
is_equal <- TRUE

for(j in 1:length(check)){
  
  sum_slow <- 0
  sum_fast <- 0
  
  for(i in 1:check[j]){
    sum_slow <- sum_slow + 1/i
  }
  
  sum_fast <- log(check[j])
  sum_slow <- round(sum_slow, 5)
  sum_fast <- round(sum_fast, 5)
  
  if(sum_slow != sum_fast){
    is_equal <- FALSE
  }
  
}  

if(is_equal == TRUE){
  cat("Results are equal")
} else{
  cat("Results are not equal\n")
  difference = sum_slow - sum_fast
  cat("Difference with n = 8000 is equal to Euler-Mascheroni constant =", (sum_slow - sum_fast))
}


