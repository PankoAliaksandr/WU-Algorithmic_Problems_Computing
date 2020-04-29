# Task 18
# For two natural numbers x and y
# write a function which outputs whether
# x is divisible by y (remainder is zero)

# Function Declaration
Check_divisibility <- function(x,y){
  ifelse(test = (x%%y == 0), yes = TRUE, no = FALSE)
}

# Main
test_vector_x <- c(2,34,15,11,7,19)
test_vector_y <- c(2,17,4,12,3,19)

for(i in 1:length(test_vector_x)){
  cat(test_vector_x[i],"divisible by", test_vector_y[i],'?',
      Check_divisibility(test_vector_x[i],test_vector_y[i]),'\n')
}

