# Task 102

# Assuming m,x >= 0, a > 0 
expmod <- function(a, x, m){
  if (m == 0){
    cat("division by 0!")
    return()
  }
  if (x == 0 & m == 1){
    return(0)
  }
  if (x == 0 & m > 1){
    return(1)
  }
  if(x == 1)
    return(a %% m)
  if (x >= 2){
    k <- x
    rec_function_38(a, k, m)
  }
}

rec_function_38 <- function(a, k, m){
  if (k > 2){
    result <- (((a %% m )* rec_function_38 (a,k - 1, m)) %% m )
  }
  if (k == 2){
    result <- ((a %% m) * (a %% m)) %% m
  }
  return (result)
}

fermat_test <- function(n, k = n/2) {
  pull <- 1:(n - 1)
  
  s <- sample(x = pull, size = k, replace = FALSE)
  
  result <- rep(F, times = k)
  
  for (i in 1:length(s)) {
    result[i] <- (expmod(a = s[i], x = n, m = n) == s[i])
  }
  
  return(!(FALSE %in% result))
}
n <- 10
k <- 6
cat(n, "can be a prime number: ",fermat_test(n,k))


# Task 103
# Show that 561 fools the Fermat primality test
cat("561 can be a prime number: ",fermat_test(561,100))

# Task 104
# Write a function "factorize" which returns the sequence of prime
# factors

factorize <- function(number){
  
  divisors <- c()
  divisor <- 2
  
  while (divisor <= sqrt(number)){
    remainder <- number %% divisor
    if (remainder == 0){
      divisors <- c(divisors, divisor) # prime divisor is added to vector of divisors
      number <- number / divisor # number is divided by its prime divisor to start factorization of quotient
      divisor <- 2 # prime factorization of quotient must be started from 2 again
    }
    else {divisor <- divisor + 1}
  }
  
  # Last divisior in number
  return(c(divisors,number))
  
}

# Test
number <- 12341234
cat('Prime factorization of',number,'is:',factorize(number), '\n')


# Task 105
# Twin primes below 1000
# Table approach

Twin_primes <- function(n) {
  
  seq <- 2 : n # all possible candidates
  primes <- integer()
  
  while (length(seq) > 0) {
    pr <- seq[1]
    primes <- c(primes, pr)
    # Delete from original sequence all definitely not primes
    seq <- seq[(seq %% pr) != 0]
  }
  
  # Now seq contains only prime numbers
  # Step 2: find twin primes
  twin_primes <- list()
  
  for (i in 2:length(primes)) {
    if ( abs(primes[i] - primes[i-1]) == 2 ) {
      pair <- c(primes[i-1], primes[i])
      twin_primes <- append(twin_primes,list(pair))
    }
  }
  return(twin_primes)
}

# Test
n <- 1000
Twin_primes(n)


# Task 106. 
fib <- function(n) {
  if (n == 1) {
    fib_vector <- c(1)
  } else if (n == 2) {
    fib_vector <- c(1, 1)
  } else {
    fib_vector <- c(1, 1, rep(0, times = (n - 2)))
    for (i in 3:n) {
      fib_vector[i] <- fib_vector[i - 2] + fib_vector[i - 1]
    }
  }
  return(fib_vector)
}

fib106 <- function(x) {
  # Step 1: Calculate required Fib. numbers:
  fib_numbers <- fib(x)
  # Calculate ratios
  number_of_ratios <- x - 1
  fib_ratios <- rep(0, times = number_of_ratios)
  for (i in 1:number_of_ratios) {
    fib_ratios[i] <- fib_numbers[i + 1] / fib_numbers[i]
  }
  return(fib_ratios)
}

# Test
fib106(31)

#The sequence seems to converge to 1.618034

#Prove it empirically based on the limit definition
epsilon <- 1e-7
golden_ratio <- (1 + sqrt(5)) / 2
fib_ratios <- fib106(31)
indexes <- which(abs(fib_ratios-golden_ratio) < epsilon)
cat("Starting with",indexes[1],"ratio, difference is less then epsilon")

# Task 107
f107 <- function(n) {
  if (n <= 1) {
    return(0)
  }
  f1 <- 1
  f2 <- 1
  counter <- 2
  while ((f1 + f2) < n) {
    counter <- counter + 1
    temp <- f2
    f2 <- f1 + f2
    f1 <- temp
    
  }
  return(counter)
}

# Test
f107(1e6)
f107(10)


# 108. Use this function to provide a function which computes the n-th Fibonacci
# number in a logarithmic number of steps.

# Function that converts the number to the binary representation string
IntToBin <- function(x) {
  if (x == 0) {
    return(c(0))
  }
  if (x == 1) {
    return(c(1))
  }
  
  mod <- x %% 2
  c(IntToBin((x - mod) %/% 2), mod)
}


# Find the n-th fibonacci number
find_fibonacci_number <- function (n)
{
  if (n == 0)
  {
    return (0)
  }
  
  if (n == 1)
  {
    return (1)
  }
  
  # T transformation matrix
  T_matrix <- cbind(c(1, 1), c(1, 0))
  
  fib_numbers <- c(1, 0) # 2 first fibonacci numbers
  
  if (n %% 2 == 0)
    # even number
  {
    k = n
  }
  
  else
    # odd number
  {
    k = n - 1
  }
  
  bin_repres <-  IntToBin(k)
  
  Result_matrix <- diag(1, 2)
  
  rev_bin_repres <- rev(bin_repres)
  
  for (i in 2:length(rev_bin_repres))
  {
    if (rev_bin_repres[i] == 1)
    {
      number_of_T_squared_transf <- i - 1
      New_matrix <-
        transform_rec_func(T_matrix, number_of_T_squared_transf)
      Result_matrix <- New_matrix %*% Result_matrix
    }
    
  }
  
  fib_numbers <- Result_matrix %*% fib_numbers
  
  if (n %% 2 == 0)
    # even number
  {
    return(fib_numbers[2])
  }
  
  else
  {
    return (fib_numbers[1])
  }
  
  
}

# Transformation T^2 recursive function (k times)
transform_rec_func <- function(T_matrix, k)
{
  T_squared_matrix <- matrix(data = 0,
                             nrow = 2,
                             ncol = 2)
  p = T_matrix[2, 2]
  q = T_matrix[1, 2]
  
  T_squared_matrix[1, 1] = p ^ 2 + 2 * p * q + 2 * q ^ 2
  T_squared_matrix[1, 2] = 2 * p * q + q ^ 2
  T_squared_matrix[2, 1] = T_squared_matrix[1, 2]
  T_squared_matrix[2, 2] = p ^ 2 + q ^ 2
  
  if (k == 1)
  {
    return (T_squared_matrix)
  }
  
  else
  {
    T_squared_matrix <- transform_rec_func(T_squared_matrix, k - 1)
  }
}

# Test
find_fibonacci_number(n = 0)
find_fibonacci_number(n = 1)
find_fibonacci_number(n = 2)
find_fibonacci_number(n = 3)
find_fibonacci_number(n = 4)
find_fibonacci_number(n = 5)
find_fibonacci_number(n = 6)
find_fibonacci_number(n = 7)
find_fibonacci_number(n = 8)
find_fibonacci_number(n = 9)
find_fibonacci_number(n = 10)
find_fibonacci_number(n = 11)
find_fibonacci_number(n = 12)



# Task 109
# Implement a function which computes, for a given positive integer n,
# all pairs (i, j) with 1 <= i < j <= n.
f_109 <- function(n) {
  pairs <- list()
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      pairs <- append(pairs, list(c(i, j)))
    }
  }
  return(pairs)
}

# Test 
f_109(6)

# Task 110 See after 115


# Task 111
directpoly <- function(coeffs, x_values) {
  #determine powers of x
  powers <- 0:(length(coeffs) - 1)
  matrix <- vector()
  
  for (i in 1:length(x_values)) {
    powers_x <- x_values[i] ^ powers
    matrix <- cbind(matrix, powers_x)
  }
  
  result_values <- coeffs %*% matrix
  #colnames(result_values) <- c("x1", "x2")
  return(result_values)
}

# Test
directpoly(c(1, 2, 3), c(1, 2))
directpoly(c(1, 2, 3, 0, 1), c(1, 0))


# Task 112

hornerpoly <- function(coeffs, x_values) {
  v <- rep(0, times = length(x_values))
  
  for (j in length(coeffs):1) {
    if (j == length(coeffs)) {
      v <- rep(coeffs[j],times = length(x_values))
    }
    else{
      v <- v * x_values + coeffs[j]
    }
    
  }
  return(v)
}

# Test
#     a1,a2,a3    x1,x2
hornerpoly(c(1, 2, 3), c(1, 2))
hornerpoly(c(1, 2, 3, 0, 1), c(1, 0))


# Task 113
# Timing comparison
# (a)
x <- seq(-10, 10, length.out = 50000)
coeff <- c(1,-2, 2, 3, 4, 6, 7)
coeff2 <- c(-3, 17, 2)
system.time(hornerpoly(x_values = x, coeffs = coeff))
# takes to much time to compute when length.out = 5 * 10^6
# so we use 5 * 10^4
system.time(directpoly(x_values = x, coeffs = coeff)) 
system.time(directpoly(x_values = x, coeffs = coeff2)) 

# (b)
# The trick is that every time you multiply by 'x', the multiply applies to 
# all terms and you needn't compute the powers of 'x' separately.

# When the number of coefficients smaller, boths methods need less iterations, 
# but 'hornerpoly' is still much faster.



# Task 114
# Write a (recursive) function count(pat, str) which counts how many times string pat
# occurs in string str. E.g., count("ab", "abab cd cdab") ) 3. (Hint: ?regexpr.)

f114 <- function(pat, str, counter = 0) {
  len_old <- nchar(str)
  
  # Delete the first pattern in the string
  str_new <- sub(pat,'',str)
  
  if (len_old != nchar(str_new)) {
    # The is pattern in the string
    counter <- counter + 1
    return(Recall(pat, str_new, counter))
  }
  else{
    # There is no pattern in the string
    return(counter)
  }
  
}

# Test
f114(pat = "st", str = "strong frost at the street")
f114(pat = "qst", str = "strong frost at the street")
f114(pat = " t", str = "strong frost at the street")
f114(pat = "ab", str = "abab cd cdab")

# Task 115
# All combinations

f115 <- function(n, k) {
  if(n == k) {
    paste0(rep(1, n), collapse = "")
  } else if (k == 0) {
    paste0(rep(0, n), collapse = "")
  } else {
    c(paste0(1, Recall(n - 1, k - 1)), paste0(0, Recall(n - 1, k)))
  } 
}

# Test 
f115(3, 2)
f115(2, 2)
f115(2, 1)
f115(3, 1)


# Task 110
# Attention 115 function is required!!!
f110 <- function(n, k) {
  x <- f115(n,k)
  all_values <- 1:n
  storage_of_comb <- list()
  for(i in 1:length(x)) {
    y <- as.integer(unlist(strsplit(x[i], "")))
    storage_of_comb[[i]] <- all_values[which(y == TRUE)]
  }
  return(storage_of_comb)
}

# Test
f110(4, 2)
f110(5, 3)


# Task 116. 
# Write a recursive function which computes the octal representation
# (as a sequence of digits 0 to 7) of a given natural number

IntToOctal <- function(x){
  if (x < 8){
    return(x)
  }
  
  mod <- x %% 8
  c(Recall((x-mod) %/% 8), mod)
}

# Test
IntToOctal(1)
IntToOctal(2)
IntToOctal(9)
IntToOctal(115)


# 119.Use a fixed-point iteration to determine the solution (in [0; 1])
# of the equation cos(x) = x. Use a starting value of 0:5.
# How many iterations does it take before you have an answer which is accurate
# in the first two, three or four digits? What happens if you change the
# starting value to 0:7 or to 0:0?

task_119_func <- function(starting_value, accuracy)
{
  x_old = starting_value
  x_new = cos(x_old)
  number_of_iterations = 1
  while (abs(x_new - x_old) >= accuracy)
  {
    x_old <- x_new
    x_new = cos(x_old)
    number_of_iterations =  number_of_iterations + 1
  }
  
  return(c(x_new, number_of_iterations))
}

# Test
result <- task_119_func(starting_value = 0.5, accuracy = 0.001)
cat("The solution", result[1], "was reached from the starting value 0.5 with", result[2], "iterations and 0.001 accuracy")
result <- task_119_func(starting_value = 0.5, accuracy = 0.0001) 
cat("The solution", result[1], "was reached from the starting value 0.5 with", result[2], "iterations and 0.0001 accuracy")
result <- task_119_func(starting_value = 0.5, accuracy = 0.00001)
cat("The solution", result[1], "was reached from the starting value 0.5 with", result[2], "iterations and 0.00001 accuracy")
result <- task_119_func(starting_value = 0.7, accuracy = 0.001) 
cat("The solution", result[1], "was reached from the starting value 0.7 with", result[2], "iterations and 0.001 accuracy")
result <- task_119_func(starting_value = 0.7, accuracy = 0.0001)
cat("The solution", result[1], "was reached from the starting value 0.7 with", result[2], "iterations and 0.0001 accuracy")
result <- task_119_func(starting_value = 0.7, accuracy = 0.00001)
cat("The solution", result[1], "was reached from the starting value 0.7 with", result[2], "iterations and 0.00001 accuracy")
result <- task_119_func(starting_value = 0.0, accuracy = 0.001)
cat("The solution", result[1], "was reached from the starting value 0.0 with", result[2], "iterations and 0.001 accuracy")
result <- task_119_func(starting_value = 0.0, accuracy = 0.0001)
cat("The solution", result[1], "was reached from the starting value 0.0 with", result[2], "iterations and 0.0001 accuracy")
result <- task_119_func(starting_value = 0.0, accuracy = 0.00001)
cat("The solution", result[1], "was reached from the starting value 0.0 with", result[2], "iterations and 0.00001 accuracy")



# Task 120
# guaranteed investment certifficate 
# interest period: 1 year
# interest type: compounded annually
# returns the amount of INTEREST earned over the term of the GIC.

GIC <- function (P, n) {
  earned_interest <- rep(0, times = n)
  for (i in 1:n) {
    if (n <= 3)
    {
      # interest rate 4%
      # interest in year 'i'
      earned_interest[i] <- P * 0.04
      # since it is compound interest
      P <- P + earned_interest[i]
    }
    else
    {
      # interest rate 5%
      # interest in year 'i'
      earned_interest[i] <- P * 0.05
      # since it is compound interest
      P <- P + earned_interest[i]
    }
  }
  return(sum(earned_interest))
}

# Test 1 
P <- 100
n <- 3
GIC(P,n)

# Test 2
P <- 100
n <- 5
GIC(P,n)


# 121.Use a fixed-point iteration with a starting guess of i = 0,006
# to calculate i. Stop the  calculation when two successive values are
# less than 0.000 001 apart.

task_121_func <- function(starting_value)
{
  x_old = starting_value
  x_new = (1- (1 + x_old)^(-20)) / 19  
  num_of_iterations <- 1
  while(abs(x_new - x_old) >= 0.000001)
  {
    x_old <- x_new
    x_new = (1- (1 + x_old)^(-20)) / 19
    print(x_new)
    num_of_iterations <- num_of_iterations + 1
  }
  
  cat("num_of_iterations =", num_of_iterations,'\n')
  return(x_new)
}

# Task
task_121_func(0.006)
task_121_func(0.004913)
task_121_func(0.003913)
task_121_func(200)

# It is clear that when our guess is closer to the root, 
# less operations required. The method converges to the same value, but the 
# number of iterations required is different

# Task 122
# Task 123 See below 124


# Task 124 
# Newton's method

# Original function
f <- function(x) {
  return(x ^ 4 + 3 * x ^ 3 - 2 * x ^ 2 - 7)
}

# Derivative
fprime <- function(x) {
  return(4 * x ^ 3 + 9 * x ^ 2 - 4 * x)
}

newton <- function(x = 1, f, fprime, epsilon) {
  n <- 0
  ## Compute function value at x.
  y <- f(x)
  while(abs(y) > epsilon ) {
    x <- x - y / fprime(x)
    y <- f(x)
    n <- n + 1
  }
  
  cat ('The zero is',sprintf("%.7f",x),'with',-log10(epsilon),
       'digits of accuracy, find in', n, 'iterations')
  return(x)
}

# Test
epsilon <- 1e-14
result124 <- newton(1,f,fprime,epsilon)


# Task 122
# NEWTON METHOD MUST BE EXECUTED in 124 first
# Original function
f122 <- function(x) {
  return(x ^ 7 + 10000 * x ^ 6 + 1.06 * x ^ 5 + 10600 * x ^ 4 + 0.0605 * x ^ 3
         + 605 * x ^ 2 + 0.0005 * x + 5)
}

# Derivative
fprime122 <- function(x) {
  return(7 * x ^ 6 + 60000 * x ^ 5 + 5.3 * x ^ 4 + 42400 * x ^ 3
         + 0.1815 * x ^ 2 + 1210 * x + 0.0005)
}

# Test
epsilon <- 1e-14
result122 <- newton(0,f122,fprime122,epsilon)



# Task 123
# NOTE: NEWTON method should be downloaded 124 task
# Original function
f123a <- function(x){
  return( (x - 3) * exp(-x) )
}
fprime123a <- function(x){
  return ( (4 - x) * exp(-x) )
}

f123b <- function(x){
  return( (x ^ 2 - 6 * x + 9)*exp(-x) ) 
} 
fprime123b <- function(x){
  return( (-(x ^ 2) + 8 * x - 15)*exp(-x) )
} 

# Test
epsilon <- 1e-7
root1 <- round(newton(2.9, f123a, fprime123a, epsilon),digits = 7)
root2 <- round(newton(2.9, f123b, fprime123b, epsilon),digits = 7)
cat("\ntime for function 1:")
system.time(newton(2.9, f123a, fprime123a, epsilon))
cat("\ntime for function 2:")
system.time(newton(2.9, f123b, fprime123b, epsilon))

# Task 125 
# Newton's method

# Original function
f125 <- function(x) {
  return(cos(x) + exp(x))
}

# Derivative
fprime125 <-function(x){
  return(-sin(x) + exp(x))
}

# Test
epsilon <- 1e-14
result125 <-newton(-1.5,f125,fprime125,epsilon)


