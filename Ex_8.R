# Task 8
# Write a function which, for a given natural number n
# returns a sequence where each i <= n is repeated i times
# in ascending order. E.g., for n = 4 the function should return
# C(1,2,2,3,3,3,4,4,4,4)

# Sequence function
seq_func <- function(n){
  result_vector <- c()
  for(i in 1:n){
    block <- rep(i,i)
    result_vector <-c(result_vector,block)
  }
  
  return(result_vector)
    
}

# Main
n <- 5
result_vector <- seq_func(n)
cat("Result vector: ", result_vector)

