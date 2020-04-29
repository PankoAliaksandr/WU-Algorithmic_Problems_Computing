# Task 17
# Write a function which for a given natural number n
# outputs all natural numbers <= n in reverse order, 
# each on a separate line (countdown)

# Assume natural numbers start from 1
# So, for 5: output: 4,3,2,1

# Function Declaration
f <- function(number){
  cat("Input number is:", number,'\n')
  for(i in number:1){
    cat(i,'\n')
  }
}

# Solution 2
f2 <- function(number) {
  cat(rev(1 : number), sep = "\n")
}

# Main
test_number = 8
cat("Solution 1:")
f(test_number)
cat("Solution 2:")
f2(test_number)



