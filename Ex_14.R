# Task 14
# Write a function which returns the geometric (or the harmonic)
# mean of two given numbers.

# Definition: 
# geometric mean of 2 numbers A and B is  sqrt(A*B)

# Definition:
# harmonic mean of 2 numbers A and B is (2*A*B)/(A+B)

# Geometric Mean Function
geometric_mean <- function(a,b){
  if(a < 0 | b < 0){
    stop("incorrect input: a and b must be >= 0")
  }
  return(sqrt(a * b))
}

# Harmonic Mean Function
harmonic_mean <- function(a,b){
  if(a <= 0 | b <= 0){
    stop("incorrect input: a and b must positive")
  }
  return((2 * a * b)/(a + b))
}

# Main

check_vector <- c(12,3,5,7,8,8)
for(i in 1:(length(check_vector)-1)){
  cat("Geometric mean of",check_vector[i],"and", check_vector[i+1]," = ",
      geometric_mean(check_vector[i],check_vector[i+1]),'\n')
  cat("Harmonic mean of",check_vector[i],"and", check_vector[i+1]," = ", harmonic_mean(check_vector[i],check_vector[i+1]),'\n')
}

