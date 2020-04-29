# Task 3
# Calculate the S = sum(i) for i=[1:n]
# compare it to n*(n+1)/2 for n = 100,200,400,800
# Use the quick formula to compute the sum for all values of n between 1
# and 100, and store the results in a vector.

check <- c(100,200,400,800)
is_equal <- TRUE

for(j in 1:length(check)){
  
  sum_slow <- 0
  sum_fast <- 0
  
  for(i in 1:check[j]){
    sum_slow <- sum_slow + i
  }
  
  sum_fast <- check[j]*(check[j]+1)/2
  sum_slow <- round(sum_slow, 5)
  sum_fast <- round(sum_fast, 5)
  
  if(sum_slow != sum_fast){
    is_equal <- FALSE
  }
  
}  

if(is_equal == TRUE){
  cat("Results are equal")
} else{
  cat("Results are not equal")}

result_vector <- c()

for(i in 1:100){
  add_value <- i*(i+1)/2   
  result_vector[i] <- add_value
}

cat("Result vector: ")
print(result_vector)