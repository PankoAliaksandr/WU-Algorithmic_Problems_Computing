# 25. The function logstar gives the smallest n such that the n-th iterate
# of the natural logarithm results in a value less than one.
# For example, logstar(123) = 3, because the second iterate 
# log(log(123)) = 1.571151 >=1 and the third iterate
# log(log(log(123))) = 0:4518085 < 1.Implement logstar.
logstar <- function(number){
  log_result <- number
  number_of_iterations <- 0
  if (number <= 0) {
    cat("Implementation of the function is impossible, since", number, 
        "is less or equal to zero. Therefore 0 iterations", "\n")
    return(0)
  }
  else{
    while (log_result >= 1){
      number_of_iterations <- number_of_iterations + 1
      log_result <- log(log_result)
    }
    return (number_of_iterations)
  }
  
}

# Test 
cat("logstar(123):",logstar(123),'\n')
cat("logstar(0):",logstar(0),'\n')
cat("logstar(-123):",logstar(-123),'\n')
cat("logstar(exp(1)):",logstar(exp(1)),'\n')


# Exercise 26. The greatest common divisor (GCD) gcd(a; b) can be obtained using Euclid's algorithm
# Write a function gcd which computes the GCD using Euclid's algorithm.
# which is based on the following two observations:
# (a) If b is a divisor of a, then gcd(a; b) = b.
# (b) If r is the remainder in the integer division of a by b, then gcd(a; b) = gcd(b; r).

# Function
gcd <- function(a,b){
  if (b == 0)
    return(abs(a))
  else
    return(abs(gcd(b,a %% b )))
}

# Test
gcd(0, 7)
gcd(7, 0)
gcd(13, 19)
gcd(36, -12)

# Excercise 27. Write a function which, for a given natural number n, returns a logical vector containing for
# each i <= n in reverse order, a sequence of length i indicating whether i is even or not. E.g.,
# for n = 3 the function should return c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE).

# Assuming that 0 is not a natural number
task_27_func <- function(n){
  vector <- c()
  for ( i in n:1){
    if (i %% 2 == 0) # even -> TRUE
      vector <- c(vector,rep(TRUE, i))
    else # odd -> FALSE
      vector <- c(vector, rep(FALSE, i))
  }
  return(vector)
}

task_27_func(3)
task_27_func(4)


# Exercise 28. Write a function which computes the number of odd numbers in a given sequence of natural
# numbers. (Hint: use sum to count the number of TRUEs in a logical vector.)
#Function
f28 <- function(natural_seq){
  sum(natural_seq %% 2 != 0)
}

# Test
f28(c(1, 2, 123, 5245, 32))
f28(c())

# 29. Write a function which returns (a logical indicating) 
# whether two given natural numbers are relatively prime (i.e., have GCD 1)
check_relatively_prime <- function(a,b){
  if (gcd(a,b) == 1)
    return (TRUE)
  else
    return (FALSE)
}

# Test
check_relatively_prime(3,7)
check_relatively_prime(6,3)
check_relatively_prime(0,-3)
check_relatively_prime(5,0)
check_relatively_prime(-6,3)

# 30. Write a function is_prime returning (a logical indicating)
# whether a given natural number is a prime number or not.
is_prime <- function(n){
  if(n == 2)
    return(TRUE)
  for(i in 2:ceiling(sqrt(n))){
    if(n %% i == 0 )
      return(FALSE)
  }
  return(TRUE)
}

is_prime(13)
is_prime(12)
is_prime(1)
is_prime(2)

# Solution 2
is_prime3 <- function(n) n == 2L || all(n %% 2L:ceiling(sqrt(n)) != 0)
is_prime3(13)
is_prime3(12)
is_prime3(1)
is_prime3(2)



# Exercise 31. Numbers of the form M_n = 2 ^ n - 1 are popular candidates
# for prime numbers. Determine the first 7 Mersenne primes by repeatedly investigating the
# next candidate until the first 7 are found.

mersenne_prime_numbers <- function(){
  prime_numbers <- c()
  n <- 1
  while(length(prime_numbers) < 7){
    next_mersenne_number <- (2^n - 1)
    if(is_prime3(next_mersenne_number) == TRUE){
      prime_numbers <- c(prime_numbers, next_mersenne_number)
    }
    n <- n + 1
  }
  return(prime_numbers)
}

# Test
mersenne_prime_numbers()


# 32. A perfect number is a positive integer which equals the sum
# of its proper divisors. The smallest perfect number is 6 = 1 + 2 + 3.
# Write a function which tests whether a given positive integer is a perfect
# number, and determine the first 4 perfect numbers via simple iteration.
is_perfect_number <- function(n){
  if (n == 1)
    return (FALSE)
  proper_divisors <- c()
  for(i in 1:ceiling(n/2)){
    if(n %% i == 0 )
      proper_divisors <- c(proper_divisors, i)
  }
  
  if(sum(proper_divisors) == n){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}

# Test
is_perfect_number(1)
is_perfect_number(2)
is_perfect_number(6)
is_perfect_number(28)
is_perfect_number(23)

# Find first 4

first_4_perfect_numbers <- function(){
  n <- 1
  perfect_numbers <- c()
  while(length(perfect_numbers) < 4){
    if(is_perfect_number(n) == TRUE){
      perfect_numbers <- c(perfect_numbers, n)
    }
    n <- n + 1
  }
  return(perfect_numbers)
}

# Test
first_4_perfect_numbers()


# 33. Write a function which computes the binary representation (as a sequence of
# 0s and 1s) of a given natural number (e.g., for x = 6 the function should
# return c(1, 1, 0)). Also implement the inverse function.

# Function that computes the binary representation of a number
intToBin <- function(x){
  if (x == 0){
    return(c(0))
  }
  if (x == 1){
    return(c(1))
  }
  
  mod <- x %% 2
  c(intToBin((x-mod) %/% 2), mod)
}

intToBin(311)
intToBin(1)
intToBin(0)


# Function that computes the decimal representation of a binary number
BinToInt <- function(x) {
  x <- as.logical(x)
  sum(2 ^ (which(rev(x))-1))
}

# Test
BinToInt(c(1,1,0))
BinToInt(c(1,1,0,1,1,0))
BinToInt(c(0,0,0))
BinToInt(c(1))


# 37. Implement a function which computes the number of ways to change a given amount
# a using k kinds of coins with denominations d1; : : : ; dk using the
# observation that this is the sum of the number of ways to change a
# without using the first kind of coin
task_37_func <- function(a, coins_vector){
  if (a < 0){
    return (0)
  }
  max_number_diff_coins <- c()
  for ( i in 1:length (coins_vector)){
    max_number_diff_coins[i] <- floor(a / coins_vector[i])
  }
  combination <- rep(0 , length (coins_vector))
  k <- 1
  result_list <- list()
  final_list <- rec_function_37 (a, coins_vector, max_number_diff_coins, k, combination, result_list)
  return (length(final_list))
}

rec_function_37 <- function (a, coins_vector, max_number_diff_coins, k, combination, result_list){
  if(k < length(coins_vector)){
    for (i in 0:max_number_diff_coins[k]){
      combination[k]<-i
      result_list <- rec_function_37(a, coins_vector, max_number_diff_coins, k + 1, combination, result_list)
    }
  }
  else if (k == length(coins_vector)){
    for (i in 0:max_number_diff_coins[k])
    {
      combination[k]<- i
      sum <- 0
      for ( y in 1:length(coins_vector)){
        sum <- ( sum + coins_vector[y]*combination[y] )
      }
      if (abs(as.double(sum) - as.double(a)) < 0.0000001){
        result_list <- append(result_list, list(combination))
      }
    }
  }
  return (result_list)
}

# 1 USD  answer [292]
task_37_func(1, c(0.5 , 0.25, 0.1, 0.05, 0.01))

# 1 EUR (takes approximately 40 sec.:)  [answer 4563]
task_37_func(1, c(2,1,0.5 , 0.2, 0.1, 0.05,0.02, 0.01))



# 38.The remainder of the (integer) division of x by m is also called
# (the remainder of) x modulo m. Verify that the remainder of x times y modulo m
# can be obtained as the remainder of the product of the remainders of x and y
# modulo m modulo m. Use this to implement a recursive function expmod which
# computes the remainder of a^x modulo m without exponentation.
# (Distinguish the cases where x is zero, even or odd.)


# Assuming m,x >= 0, a > 0 
task_38_func <- function(a, x, m){
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


# Test
task_38_func(36 , 7 ,21)
task_38_func(5 , 2 ,6)
task_38_func(4 , 0 ,1)
task_38_func(4 , 0 ,3)
task_38_func(4 , 0 ,0)


# 39. What are the smallest and largest 32-bit integers representable by
# the biased scheme?

cat("Smallest 32-bit integer representable by the biased scheme:", 1-2^31)
cat("Largest 32-bit integer representable by the biased scheme:", 2^31)


# 40 PLEASE FIND IT AFTER 41

# 41.Write a function which computes the k-bit (default: k = 32) two's complement
# representation of a given integer. Also implement the inverse function

to_twos_complement_representation <- function(n, k = 32){
  
  if (n >= 0){ # positive n
    binary_representation <- intToBin(n)
    twos_complement_representation <- binary_representation
    
    if (k < length (twos_complement_representation))
    {
      cat ("It is impossible to represent ", n, " with ", k, "bits.")
    }
    else{
      twos_complement_representation_k_bit <- c( rep (0, k - length(twos_complement_representation)), twos_complement_representation)
      return (twos_complement_representation_k_bit)
    }
  }
  
  
  else{   # negative n
    
    binary_representation <- intToBin(abs(n))
    twos_complement_representation <- rep(0, length(binary_representation))
    
    meet_left_1 <- FALSE
    for (i in length(binary_representation):1){
      
      if(meet_left_1 == TRUE){
        if (binary_representation[i] == 1)
          twos_complement_representation[i] <- 0
        else
          twos_complement_representation[i] <- 1
      }
      
      else{ # flag meet_left_1 is FALSE
        
        if(binary_representation[i] == 1){
          meet_left_1 <- TRUE
          twos_complement_representation[i] <- 1
        }
        else{
          twos_complement_representation[i] <- binary_representation[i]
        }
        
      }
      
    }
    
    if (k < length (twos_complement_representation))
    {
      cat ("It is impossible to represent ", n, " with ", k, "bits.")
    }
    else{
      twos_complement_representation_k_bit <- c( rep (1, k - length(twos_complement_representation)), twos_complement_representation)
      return (twos_complement_representation_k_bit)
      
    }
    
  }
  
}

# TEST
to_twos_complement_representation(-30, 15)
to_twos_complement_representation(-30)
to_twos_complement_representation(-30, 2)
to_twos_complement_representation(0, 32)
to_twos_complement_representation(23, 12)
to_twos_complement_representation(24, 2)


# Function Definition

from_twos_complement_representation <- function(twos_complement_repr){
  
  if (twos_complement_repr[1] == 0) # positive integer
  {
    binary_representation <- twos_complement_repr
    decimal_repr <- BinToInt(binary_representation)
  }
  else # negative integer
  {
    binary_representation <- rep(0, length(twos_complement_repr))
    
    meet_left_1 <- FALSE
    for (i in length(twos_complement_repr):1){
      
      if(meet_left_1 == TRUE){
        if (twos_complement_repr[i] == 1)
          binary_representation[i] <- 0
        else
          binary_representation[i] <- 1
      }
      
      else{
        
        if(twos_complement_repr[i] == 1){
          meet_left_1 <- TRUE
          binary_representation[i] <- 1
        }
        else{
          binary_representation[i] <- twos_complement_repr[i]
        }
        
      }
      
    }
    
    decimal_repr <- -1 * BinToInt(binary_representation)
  }
  
  return (decimal_repr)
  
}

from_twos_complement_representation (c(0,0,0,0,0,0,0,0))
from_twos_complement_representation (c(1,1,1,1,1,1,1,1))
from_twos_complement_representation (c(0,0,0,1,1,1,1))
from_twos_complement_representation (c(1,1,1,0,0,1,1))


# 40 Using the two's complement representation for signed integers, what are the numbers cor-
# responding to the k-bit sequences 1,1.... 1 and 10....01, respectively?

# 1...1 (k bit) = -1
# 10....01 (k bit) = 1 - 2^(k-1)

# Test
from_twos_complement_representation (c(1,1,1,1,1,1,1,1))
from_twos_complement_representation (c(1,0,0,0,0,0,0,1))



# 42. Rational numbers are fractions of the form p=q with integers p and q > 0
# satisfying gcd(p; q) = 1. We can represent such numbers as sequences c(p, q) of
# length two. Write a function make_rat which for two given numbers p and q
# returns the corresponding rational number as a sequence of length 2
# (e.g., make_rat(19, -95) ) c(-1, 5).) Also write functions numer and denom
# for extracting the numerator and denominator of such a rational number.

make_rat <- function(a, b){
  if( b == 0){
    cat("Division by 0!")
    return()
  }
  gcd_value <- gcd(a, b)
  a <- a / gcd_value
  b <- b / gcd_value
  sign_value <- sign (a*b)
  rational_number <- c(sign_value*abs(a), abs(b))
  return (rational_number)
}

make_rat(19,-95)
make_rat(17,51)
make_rat(-19,17)
make_rat(-3,-15)
make_rat(0,-9)
make_rat(19,0)

numer <- function(a,b){
  rational_number <- make_rat(a,b)
  return(rational_number[1])
} 

# TEST
numer(19, -95)

denom <- function(a,b){
  rational_number <- make_rat(a,b)
  return(rational_number[2])
} 

# TEST
denom(19, -95)

# Write functions add_rat and mul_rat for the rational addition and
# multiplication of rational numbers

# Function Definition 
add_rat <- function(rat_number_1, rat_number_2){
  denominator <- rat_number_1[2]*rat_number_2[2]
  numerator <- rat_number_1[1]*rat_number_2[2] + rat_number_2[1]*rat_number_1[2]
  rat_value <- make_rat(numerator, denominator)
  return(rat_value)
}

# TEST
add_rat(c(-3,4), c(21,12)) 

# Function Definition 
mul_rat <- function(rat_number_1, rat_number_2){
  numerator <- rat_number_1[1]*rat_number_2[1]
  denominator <- rat_number_1[2]*rat_number_2[2]
  rat_value <- make_rat(numerator, denominator)
  return(rat_value)
}

# TEST
mul_rat(c(-3,4), c(21,12)) 


# 44
# Consider a floating-point number system with base b, precision p, 
# and exponent range [emin; emax].
# (a) How many different non-zero normalized numbers are there?
# (b) What are the smallest (normalized) and largest positive numbers?

# RESULTS:
# (a): 2*(B-1)*B^(p-1)*(Emax - Emin + 1)
# (b): Smallest -> B^Emin
#      Largest  -> (1-B^(-p))*B^(Emax + 1)


# 45
# Consider a floating-point number system with base b, precision p, and
# exponent range [emin; emax]. If b = 10, what are the smallest values of p
# and emin and largest value of emax  such that both 2365.27 and 0.0000512
# can be represented exactly as normalized floating-point numbers?


# RESULTS:
# Emin = -5
# Emax = 3
# p = 6


# 46
# In a floating-point number system with precision p = 6 decimal digits,
# let x = 1.23456 and y = 1.23579. 
# (a) How many significant digits does the difference y-x contain?
# (b) What is the minimum exponent range for which x, y and y-x are
#     all exactly representable as normalized numbers?

# RESULTS:
# (a) 4 significant digits
# (b) [-3,0]


# Task 49
# In many floating-point systems, a quick approximation to the unit
# roundoff error can be obtained by the approximation e = abs(3*(4/3-1)-1)


# Explanation
# Step 1: 4/3 = 1.333.... = 1.(3) 
# [in a period maximum possible didgits(after dot) in mantisa]
# Step 2: 1.(3) - 1 = 0.(3)
# Step 3: 3*0.(3) = 0.(9)
# Here 0.(9) as close as possible in the system to 1
# 0.(9) - 1 = gives us roundoff error of the system


# TASK 50

# Can you explain these two results?
# Initialization
x <- c(0, 7, 8)

# Test 1
cat("Result of test 1:", x[0.9999999999999999],'\n')
# Test 2
cat("Result of test 2:", x[0.99999999999999999],'\n')

# Explanation

# Step 1
#   According to the IEEE_754-1985 standard
#   our precision is only 16 digits after a comma (dot)
# Step 2 (Test 1)
#   In test 1 there are 16 digits precision => the difference can be measured
index = 0.9999999999999999
if (index == 1){
  cat("index is equal 1")
}else{
  cat("index is not equal 1")
}
# According to the documentation x[0.9999999999999999] is equal to
# x[as.integer(0.9999999999999999)]
# as.integer rounds the argument down, 
# so every number that less than 1 are rounded to 0
# Since Test 1 index less than 1 => we have x[0]
# since indexing starts with 1 it gives us numeric(0)

# Step 2 (Test 2)  
#   Here precision is 17 digits => R cannot see the difference from 1
#   Let's prove that index = 1: compare the index and 1
index = 0.99999999999999999
if (index == 1){
  cat("index is equal 1")
}else{
  cat("index is not equal 1")
}
# So we have x[1] = 0



# 51
# Write functions which use a starting value of 1 and
# repeated division by 2 to find the smallest numbers 'e' 
# such that 1 + e != 1 and 1 - e != 1, respectively. What are these numbers?

# Function can be optimized using only 1 loop but it will make it more difficult
# to check and read
Find_min_numbers <- function(){
  
  number_plus <- 1
  number_minus <- 1
  while(TRUE){
    if(1 + number_plus!= 1){
      number_plus = number_plus/2
    }else{
      break()
    }
  }
  
  
  cat(" the smallest number 1 + e != 1 is:", number_plus,'\n')
  
  while(TRUE){
    if(1 - number_minus!= 1){
      number_minus = number_minus/2
    }else{
      break()
    }
  }
  cat(" the smallest number 1 - e != 1 is:", number_minus,'\n')
  
}

# Main
Find_min_numbers()


# Task 52
# Consider the IEEE 754 double-precision  floating-point system.
# What are the smallest positive normalized and the largest positive denormalized
# numbers? What is the difference between them?

# Smallest positive normalized
# since the number is normalized d0 = 1 (no bit used to store it)
# we have additionally 52 bits for mantisa - all zeros
# the minimal available power is -1022 (since -1023 means that number is denormalized)
# All this give as a formula: 1 * 2^-1022
smallest_pos_norm_num <- 1*2^-1022
cat("Smallest positive normalized number is", smallest_pos_norm_num, '\n')

# The largest positive denormalized
# Denormolized => d0 = 0
# d1...d52 = 1
# Since denormolized, exponent all zeros
# All this give as a formula: 0 + (1/2 - (1/2)^-52)/(1 - 1/2) * 2^-1022
largest_pos_denorm_num <- (0 + (1/2 - (1/2)^-52)/(1 - 1/2)) * 2^-1022
cat("Largest positive denormalized number is", largest_pos_denorm_num, '\n')

# Answer is (1-2^-52) * 2^-1022



