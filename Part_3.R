# 54-65, 67, 70-74

# Task 54
# Find the largest positive floating-point number
# representable by the IEEE 754 64-bit standard.

# Explanation: 
# number = ( 1 + 1/2 + ... 1/2^52 ) * 2^1023

cat("The required number is", 2*(1-2^(-53))*2^1023 )


# Task 55
# Find the smallest normalized positive IEEE 754 64-bit
# floating-point number for which 1 + eps != 1.

cat("The required number is", 2^(-52) )

# Test
1 == 1 + 2^(-52)


# Task 56
# Find the smallest normalized positive IEEE 754 64-bit
# floating-point number for which 1 - eps != 1.

cat("The required number is", 2^(-53) )

# Test
1 == 1 - 2^(-53)


# Task 57
# Write a function computing the coefficients of the "best" dyadic rational
# approximation of length k to a given number x in [0; 1].

dyadic_rational_approximation <- function(x, k)
{
  if(x < 0 | x > 1)
  {
    print("x must be in the interval [0,1]")
    return()
  }
  
  if(k < 1)
  {
    print("k must be greater or equal to 1")
    return()
  }
  
  result <- rep(x = 0,times = k)
  
  for (i in 1:k)
  {
    if (x >= 1/(2^i))
    {
      result[i] = 1
      x = x - 1/(2^i)
    }
    else
    {
        result[i] = 0
    }
  }
  
  return (result)
}

# Test
dyadic_rational_approximation(x = 3/8, k = 4)
dyadic_rational_approximation(x = 0, k = 7)
dyadic_rational_approximation(x = -4, k = 7)


# Task 58
# Suppose x and y can be represented without error in double precision.
# Can the same be said for x2 and y2? Which would be more accurate,
# For what values of x and y, relative to each other, is there a substantial
# difference in the accuracy of the two expressions?

# Question 1:
# It is more accurate to evaluate (x^2 - y^2) using (x - y)(x + y) formula.
# This improved form still has a subtraction, 
# but it is a benign cancellation of quantities without rounding error
# Multiplying two quantities with a small relative error results.

# Question 2:
# Substantial difference in the accuracy will be for such
# x and y which x^2 and y^2 could not be represented without error. 
# For example, x^2 or y^2 are larger than the largest double
# precision floating point number.

# Test
x <- 2*(1-2^(-53))*2^512
y <- 2*(1-2^(-53))*2^512
cat( "x^2-y^2 ==>", x^2-y^2) 
cat("(x+y)*(x-y) ==>",(x+y)*(x-y))

# Task 59
# Find three floating-point numbers a, b and c for which addition
# is not associative, i.e., for which (a + b) + c != a + (b + c).

a <- 1.65
b <- 2^(-53)
c <- 2^(-54)

a + (b+c) == (a+b)+c


# Task 60
# The natural logarithm and exponential functions are inverses of each other,
# so that mathematically log(exp(x)) = exp(log(x)) = x.
# Show by example that this property does not hold exactly
# in computer arithmetic. Does it hold "approximately"?

# Example
x <- 2^(-53)
x == log(exp(x))
x == exp(log(x))
log(exp(x)) == exp(log(x))

# It holds approximately
x - log(exp(x))
x - exp(log(x))
log(exp(x)) - exp(log(x))


# Task 61
# Calculate e - the base of natural logarithms

# Calculate 20 eulers numbers
Eulers_number <- function(n) {
  e <- (1 + 1 / n) ^ n
  return(e)
}

eulers_numbers_vector <- rep(x = 0, times = 20)
for(k in 1 : 20) {
  n <- 10 ^ k 
  eulers_numbers_vector[k] <- Eulers_number(n)
}

# Determine the error

vector_of_abs_errors <- abs(eulers_numbers_vector - exp(1)) # Absolute errors
for(i in 2:k){
  if(vector_of_abs_errors[i] >= vector_of_abs_errors[i-1])
  {
    cat("k =", i, "Error does not decrease\n")
  }
  else
  {
    cat("k=", i, "Error decreases\n")  
  }
}


# Explanation
# Since R uses finite precision arithmetic, in base 10,
# double precision is roughly equivalent to 16 significant figures, with exponents of size up to +308 or - 308.

# Another problem is that numbers are represented in this format as a whole number times a power of two; 
# rational numbers (such as 0.1, which is 1/10) whose denominator is not a power of two cannot be exactly represented.

# A long series of calculations will usually result in larger errors than a short one.

# From k = 16 and on, 1 / n can't be distinguished from 1, meaning
# 1 /n < 2^(-52) =(about) 2.220446*10^16 (machine epsilon).

# For k = 15. Since, only  the 15-16 significat digits are stored exactly and others are not, (we can check it using sprintf), 
# multiplicating them 10^15 times lead to huge roundoff error, resulting even in changing d0 to 3.
# However, the truncation error is lower.

# For other numbers the same reasoning is applicable, but since the exponent is not so great the results USUALLY have 
# lower roundoff error, but bigger truncation error. Therefore, taking into account our vector of relative errors
# and absolute errors, we see where the the roundoff error is the largest.



# Task 62
# f(x) = (e ^ x - 1) / x
# a)
# Using L'Hopital's rule, lim(f(x)) [x -> 0] = lim(exp^x/1) [x -> 0] = 
# = exp(0) / 1 = 1

# b)
function_values <- rep(x = 0, times = 15)
for(k in 1 : 15) {
  x <- 10 ^ (-k)
  function_values[k] <- (exp(x) - 1) / x
}

#Absolute Errors
vector_of_abs_errors <- abs(function_values - 1)
for(i in 2:k){
  if(vector_of_abs_errors[i] >= vector_of_abs_errors[i-1])
  {
    cat("k =", i, "Error does not decrease\n")
  }
  else
  {
    cat("k=", i, "Error decreases\n")  
  }
}

# Explanation. 
# The reasoning is the same as in previous example. 10 ^ (-k), (exp(1)) ^ x, and the whole division can't be presicely
# represented. Therefore, due to the floating pointiong point representation, the numbers are rounded on each step of
# the calculations up to roughly 16 decimal digits. Moreover, the trucation error appears in each step of our calculations,
# meaning that we were getting rid of other calculations, which will (probably) finally lead to 1. E.g.: When calulating
# the result for k = 15, we chop off k = 16, 17, etc.

# Taking into account our vector of absolute errors, we can see that roundoff error decreases from k=1 to k=8, reaching
# its minimal value, while afterwards it increases again.



#c)
function_values_2 <- rep(x = 0, times = 15)
for(k in 1 : 15) {
  x <- 10 ^ (-k)
  function_values_2[k] <- (exp(x) - 1) / log(exp(x))
}

#Absolute Errors
vector_of_abs_errors_2 <- abs(function_values_2 - 1)
for(i in 2:k){
  if(vector_of_abs_errors_2[i] >= vector_of_abs_errors_2[i-1])
  {
    cat("k =", i, "Error does not decrease\n")
  }
  else
  {
    cat("k=", i, "Error decreases\n")  
  }
}

# Explanation. We can clearly see from absolute errors vector that this
# approximation works way better, with gradual decrease of absolute errors,
# while k increases.  That's because symbolic computations are not so prone 
# to round-off errors, and as we first compute e ^ x and than  take log of it,
# the round-off error becomes lower.

# Task 63
# Write a program to compute an approximate value for the derivative
# of a function using the finite-difference formula.

calculate_derivative <- function(x, f,h)
{
  derivative <- (f(x+h) - f(x))/h
  return(derivative)
}

# Test
h <- 2^(-40)
cat("derivative approximated:",calculate_derivative(1,tan,h), '\n')
cat("derivative using the given formula", 1+tan(1)^2,'\n')

# Test 2:
h <- 2^(-52)
cat("derivative approximated:",calculate_derivative(1,tan,h), '\n')
cat("derivative using the given formula", 1+tan(1)^2,'\n')


# Explanation of why our approximation does not work.
# We have such a result, because of so called cancellation error, meaning that
# subtraction of nearly equal operands may cause extreme loss of accuracy.
# That's exactly the case in our formula. 
# As we decided to plug h very close to zero, then, when using floating-point operations, 
# the smallest number won't give the best approximation of a derivative. 
# As h grows smaller the difference between f (x + h) and f(x) grows smaller, 
# cancelling out the most significant and least invalid digits and making the most invalid digits more important. 
# As a result the smallest number of h possible will give a more invalid approximation 
# of a derivative than a somewhat larger number. 



# Task 64.

# Suppose that b > 0. Then, in computing x1, we encounter enormous cancellation if b is much
# larger than a and c, because this implies that sqrt(b^2 − 4ac) approximately equals b and as a result we are subtracting
# two numbers that are nearly equal in computing the numerator. On the other hand, if b < 0, we
# encounter this same difficulty in computing x2.

# Considering the modified formula, it is is not susceptible to catastrophic cancellation,
# because an addition is performed instead of a subtraction.




# Task 65
# There is a danger of overflow when x is close to 1 or -1. Another danger is cancellation error when x 
# is very close to 0, as we are substracting numbers of almost equal size.

# To avoid cancellation near 0, we can easily rearrange our initial difference by
# finding common denominator and substracting them, which leads to
# 2x / (1 - x ^ 2)


# Task 67
# How would you avoid overflow and harmful underflow in this computation?

calculate_euclidean_norm <- function (vector)
{
  # step 1: find maximum coordinate of the vector
  max_abs_value <- max(abs(vector))
  # step 2: divide all coordinates by the maximum coordinate
  vector <- vector / max_abs_value
  # step 3: calculate the norm with formula below
  euclidean_norm <- max_abs_value * sqrt(sum(vector^2))
  return(euclidean_norm)
}  


# Test
calculate_euclidean_norm(c(0,4,-3))


# Task 70

# To calculate log(x) using the expansion for log(1+x)
# How many term are required to calculate log(1.5) and log(2) with error < 10^(-16)

num_of_terms <- function(x,precision, multiplier){
  # the error is no greater in magnitude than the last term in the sum.
  last_term <- x
  i <- 1
  while(abs(last_term * multiplier) > precision){
    last_term <- (x^i)/i
    i <- i+1
  }
  cat("Absolute value of the last term:", abs(last_term * multiplier),'\n')
  cat("Number of iterations required:", i-1,'\n')
}


num_of_terms(0.5, 10^(-16),1)
x <- sqrt(2)-1
num_of_terms(x, 10^(-16),2)


# Task 71

rec_function_71 <- function (x){
  
  if(abs(x) >= 0.1)
  {
    result <- 3 * rec_function_71(x/3) - 4 * ( rec_function_71(x/3) )^3
  }
  else
  {
    result <- x
  }
  
  return (result)
}


# Test
rec_function_71(1)
sin(1)

# calculate number of steps
# we assume number of steps to be equal the depth of recursion
# Use the function below with the same X value to calculate the number of 
# step required.

calculate_number_of_steps <- function(x){
  number_of_steps <- 0 
  while(x >= 0.1){
    x <- x/3
    number_of_steps <- number_of_steps + 1
  }
  return(number_of_steps)
}

# Test: 
cat("Number of steps:", calculate_number_of_steps(1))


# Task 72

# The fisrt approach - using the infinitely decreasing geometric progression formula
sprintf("%.20f",1/9,'\n')

# The second approach
# Calculated from the end to save the precision
# and the last term must be 2^(-1074) - the min possible floating-point number
number_of_steps <-floor(log(x = 2^(-1074), base = 0.1))
sum <- 0
for(i in number_of_steps:1)
{
  sum <- sum + (1/10)^i
}
sprintf("%.20f",sum)


# Task 73
# The sum in the exercise can be written as:
# sum from 1 to infinity 1/(n*(n+1)) and n <- n+2
# we must find such n that 1/(n*(n+1)) <= 2^(-1074)
# after solving the inequation we have a result n is about 2^536
# (it is the number of iterations that are
#  required to calculate as accurate as possible)
# Unfortunately, this number of iterations is too big to be calculated in 
# reasonable time.
# Moreover, to provide the best accuracy the sum must be summarized from the end


# Task 74
# This case is absolutely the same as Task 73
# sum from 1 to infinity 1/(n*(n+2)) and n <- n+4
# Unfortunately, the number of iterations is too big to be calculated in 
# reasonable time. 
# Moreover, to provide the best accuracy the sum must be summarized from the end

