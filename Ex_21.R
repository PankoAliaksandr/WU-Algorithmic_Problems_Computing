# Task 21
# Write a function which takes three numbers as arguments
# and returns the sum of the squares of the two larger numbers.

# Function Declaration
Large_sum <- function(a,b,c){
  array <- c(a,b,c)
  sorted <- sort(x = array, decreasing = TRUE)
  two_max <- c(sorted[1], sorted[2])
  sum_squared <- two_max[1]^2 + two_max[2]^2
  return(sum_squared)
}


# Main
a = 5
b = -3
c = -5

cat("Sum of the squares of the two larger numbers is",
    Large_sum(a,b,c))

