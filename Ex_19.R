# Task 19
# Write a function which computes the sum of the digits
# of a given natural number.

# Function Declaration
Digit_sum <- function(number){
  sum <- 0
  while(number > 0){
    digit <- number%%10
    sum <- sum + digit
    number <- number%/%10
  }
  return(sum)
}


# Main
check_vector <- c(4,14,324,7643,989021)
for(i in 1:length(check_vector)){
  cat("Sum of digit in", check_vector[i],"equals to",
      Digit_sum(check_vector[i]), '\n')
}

