# Task 20
# Write a function which computes the total number of digits
# in a given sequence of natural numbers.

# Function Declaration
Count_total_num_digits <- function(seq_num){
  char_seq <- paste(seq_num, collapse = '')
  digits_num <- nchar(char_seq)
  return(digits_num)
}

# Solution 2
Count_total_num_digits_2 <- function(seq_num) {
  sum(nchar(as.character(seq_num)))
}

# Main
check_vector <- c(2,14,23433,432,1231,23123)
cat("The total number of digits is (solution 1)",
    Count_total_num_digits(check_vector),'\n')
cat("The total number of digits is (solution 2)",
    Count_total_num_digits_2(check_vector),'\n')

