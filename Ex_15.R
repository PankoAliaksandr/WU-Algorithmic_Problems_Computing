# Task 15
# Write a function which outputs which of two given character strings is shorter.

# Shortest Function
Find_shorter <- function(strings) {
  size_vector <- nchar(strings)
  min_index <- which.min(size_vector)
  return(min_index)
}


# Main
str <- c("aa", "bbb", "333", "55555", "h", "*#!~")
shortest <- Find_shorter(str)
cat("The shortest string is:",str[shortest])
