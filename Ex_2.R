# Task 2
# Calculate the S = sum(r^i) for i=[1:n]
# compare it to (r^(n+1)-1)/(r-1)-1 for n = 10,20,30,40
# Use the quick formula to compute the sum for all values of n between 1
# and 100, and store the results in a vector.

r <- 1.08
check <- c(10,20,30,40)
is_equal <- TRUE

for(j in 1:length(check)){
  
  sum_slow <- 0
  sum_fast <- 0
  
  for(i in 1:check[j]){
    sum_slow <- sum_slow + r^i
  }
  
  sum_fast <- (r^(check[j]+1)-1)/(r-1)-1
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
  add_value <- (r^(i+1)-1)/(r-1)-1   
  result_vector[i] <- add_value
}

cat("Result vector: ")
print(result_vector)
