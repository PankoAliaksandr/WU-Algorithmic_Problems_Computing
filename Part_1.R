# Task 4
# Calculate the S = sum(i^2) for i=[1:n]
# compare it to n*(n+1)*(2n+1)/6 for n = 200,400,600,800
# Use the quick formula to compute the sum for all values of n between 1
# and 100, and store the results in a vector.

check <- c(200,400,600,800)
is_equal <- TRUE

for(j in 1:length(check)){
  
  sum_slow <- 0
  sum_fast <- 0
  
  for(i in 1:check[j]){
    sum_slow <- sum_slow + i^2
  }
  
  sum_fast <- check[j]*(check[j]+1)*(2*check[j]+1)/6
  sum_slow <- round(sum_slow, 5)
  sum_fast <- round(sum_fast, 5)
  
  if(sum_slow != sum_fast){
    is_equal <- FALSE
  }
  
}  

if(is_equal == TRUE){
  cat("Results are equal")
} else{
  cat("Results are not equal")}

result_vector <- c()

for(i in 1:100){
  add_value <- i*(i+1)*(2*i+1)/6   
  result_vector[i] <- add_value
}

cat("Result vector: ")
print(result_vector)


# Task 5
# Calculate the S = sum(1/i) for i=[1:n]
# compare it to log(n) for n = 500,1000,2000,4000,8000.
# Use the quick formula to compute the sum for all values of n between 1
# and 100, and store the results in a vector.

check <- c(500,1000,2000,4000,8000)
is_equal <- TRUE

for(j in 1:length(check)){
  
  sum_slow <- 0
  sum_fast <- 0
  
  for(i in 1:check[j]){
    sum_slow <- sum_slow + 1/i
  }
  
  sum_fast <- log(check[j])
  sum_slow <- round(sum_slow, 5)
  sum_fast <- round(sum_fast, 5)
  
  if(sum_slow != sum_fast){
    is_equal <- FALSE
  }
  
}  

if(is_equal == TRUE){
  cat("Results are equal")
} else{
  cat("Results are not equal\n")
  difference = sum_slow - sum_fast
  cat("Difference with n = 8000 approximately equals to Euler-Mascheroni constant =", (sum_slow - sum_fast))
}





# Task 6
# Using seq() and rep() as needed, create the vectors
# Sequence 1: 0 0 0 0 0 1 1 1 1 1 2 2 2 2 2 3 3 3 3 3 4 4 4 4 4 and 
# Sequence 2: 1 2 3 4 5 1 2 3 4 5 1 2 3 4 5 1 2 3 4 5 1 2 3 4 5

result_vector <- rep(seq(from = 0,to = 4), each = 5)
cat(" Sequence 1: ", result_vector)
result_vector <- rep(seq(from = 1,to = 5), times = 5)
cat(" Sequence 2: ", result_vector)


# Task 7
# Using seq() and rep() as needed, create the vectors
# Vector: 1 2 3 4 5 2 3 4 5 6 3 4 5 6 7 4 5 6 7 8 5 6 7 8 9

result_vector <- seq(from = 1,to = 5) + rep(seq(from = 0,to = 4), each = 5)
cat(" Vector: ", result_vector)


# Task 8
# Write a function which, for a given natural number n
# returns a sequence where each i <= n is repeated i times
# in ascending order. E.g., for n = 4 the function should return
# C(1,2,2,3,3,3,4,4,4,4)

# Sequence function
seq_func <- function(n){
  result_vector <- c()
  for(i in 1:n){
    block <- rep(i,i)
    result_vector <-c(result_vector,block)
  }
  
  return(result_vector)
  
}

# Main
n <- 5
result_vector <- seq_func(n)
cat("Result vector: ", result_vector)



# Task 9
# Create a vector called "numbers" : c(3,5,8,10,12)
# Dump "numbers" to a file called `numbers.R'
# delete "numbers" using rm().
# Using ls(), confirm that "numbers" has been deleted.
# Now, use source() to retrieve the vector numbers.

numbers <-c(3,5,8,10,12)
name <-"numbers"
cat("'",name,"'", "vector is created: ", numbers)
# Save the variable in a file
dump(name, file = "numbers.R" )
cat("'",name,"'", "vector has been dumped")
# Delete the variable
rm(numbers)
# Retrieve available variables
variables <-ls()
if  (name %in% variables){
  cat("Variable '", name, "' is available:")
}else{
  cat("Variable '", name, "' has been removed")
}

# Download saved variable
source_vector <- source(file = "numbers.R")
variables <-ls()
if  (name %in% variables){
  cat("Variable '", name, "' is available\n")
  print(source_vector[1])
}else{ 
  cat("Variable '", name, "' has been removed")
}


# Task 10
# Write a function which saves the 26 upper-case letters
# of the Roman alphabet to a given file.

# Saving Function
Save_Roman_alphabet <- function(file_name){
  write(LETTERS, file = file_name )
  cat("Uppercase alphabet has been saved in file '",file_name,"'")
}

# Main
file_name <- "Alphabet.txt"
Save_Roman_alphabet(file_name)
sourve_vector <- readLines(file_name)
cat("Downloaded alphabet")
print(sourve_vector)




# Task 11
# Plot the graph of the function
# f(x) = 3*x + 2 , x<=3
# f(x) = 2*x - 0.5 * x^2, x >3
# interval [0,6]

f <- function(x){
  ifelse(x <=3 , 3*x+2, 2*x - 0.5*x^2) 
}
curve(expr = f,from = 0,to = 6,main = 'Function Plot', xlab = 'argument x',
      ylab = 'F(x)',col = "red",xlim = c(0,6),ylim = c(-10,20))




# Task 12
# Write a function which outputs whether a given number is positive or negative.

# Check Function
Check_sign <- function(x){
  if(x == 0){
    return(0)
  }
  
  ifelse(test = x > 0, yes = return(1), no = return(-1))
  
}

# Main
check_verctor <- c(-4,5,0)

for(i in 1:length(check_verctor)){
  
  if(Check_sign(check_verctor[i]) == 1){
    cat("value",check_verctor[i],"is positive\n")
  }
  if(Check_sign(check_verctor[i]) == -1){
    cat("value",check_verctor[i],"is negative\n")
  }
  if(Check_sign(check_verctor[i]) == 0){
    cat("value",check_verctor[i],"is 0\n")
  }
  
}



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


# Task 16
# Write a function which outputs a given character string once
# for each character in the string.
# (Hint: ?"for".)

# Function Declaration
f <- function(word){
  rep_num <- nchar(word)
  cat("The word is:", word,"\tNumber of letters =", rep_num,'\n')
  for(i in 1:rep_num){
    cat(i,')',word,'\n')
  }
}

# Solution 2
f2 <- function(word) {
  rep(word, times = nchar(word))
}

#Main
test_word = "four"
f(test_word)
cat("Solution 2\n")
f2(test_word)



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



# Task 22
# Write a function which uses method (in real arithmetic)
# to compute the unique real root of a cubic equation

# Function Declaration
cuberoot <- function(x)sign(x)*abs(x)^(1/3)

# Function Declaration
# Arguments are coefficients of cubic equation
Find_root <- function(a,b,c){
  # Step 1: substitution y = x + a/3
  # calculate p and q using next formulas
  p <- (3*b - a^2)/3
  cat("p = ", p,'\n')
  q <- ((2*a^3)/27 - a*b/3 + c)
  cat("q = ", q,'\n')
  
  # Step 2: Calculate Discriminant using next formula
  D <- (p/3)^3 + (q/2)^2
  cat("D=",D,'\n')
  
  # Step 3: Check discriminant (must be POSITIVE according to task)
  if(D > 0){
    sqrt_D <- sqrt(D)
    x <- (-a/3 + cuberoot(-q/2 + sqrt_D) + cuberoot(-q/2 - sqrt_D))
    cat("There is only one real root:", x,'\n')
  }else{
    cat("There are no real roots or the root is non-unique\n")
  }
  
}

# Main
a <- 2
b <- 8
c <- 0

Find_root(a,b,c)



# Task 23
# Write a function factorial to compute n!using prod().
# Compute 10!, 50!, 100!, and 1000!.
# Use factorial to write a function which computes the binomial coefficient
# Compute C(4,2), C(50,20), C(5000,2000)
# Implove formula using sum() and exp()
# Compute C(4,2), C(50,20), C(5000,2000)


# Function Definition Factorial
Fact <- function(n){
  numbers <- seq(n)
  result <- prod(numbers)
  return(result)
}

# Function Definition Big N Factorial
Fact_big <- function(n){
  seq_x <- 1:n
  seq_log_x <- log(seq_x)
  sum_log <- sum(seq_log_x)
  result <- exp(sum_log)
  return(result)
}

# Function Definition Binomial
Binom_coeff_big <- function(a,b){
  return(Fact_big(a)/(Fact_big(b)*Fact_big(a-b)))
}

# Function Definition Binomial
Binom_coeff <- function(a,b){
  return(Fact(a)/(Fact(b)*Fact(a-b)))
}


# Main 
array <- c(10,50,100,1000)
for(i in 1:length(array)){
  cat("factorial",array[i],"is =",Fact(array[i]),'\n') 
}

for(i in 1:length(array)){
  cat("factorial using improved function",array[i],"is =",Fact_big(array[i]),'\n') 
}

binom_a  <- c(4,50,5000)
binom_b  <- c(2,20,2000)
for(i in 1:length(binom_a)){
  cat("Binomial coefficient C(",binom_a[i],',',
      binom_b[i],") is", Binom_coeff(binom_a[i],binom_b[i]),'\n')
}

for(i in 1:length(binom_a)){
  cat("Binomial coefficient using improved function C(",binom_a[i],',',
      binom_b[i],") is", Binom_coeff_big(binom_a[i],binom_b[i]),'\n')
}



# Task 24
# Write a function to compute

n <- c(50,100,150,200,250)
a <- lgamma((n-1)/2)
b <- lgamma(1/2)
c <- lgamma((n-2)/2)

orig_a <- exp(a)
orig_b <- exp(b)
orig_c <- exp(c)
ro <-orig_a / (orig_b * orig_c)

cat("ro =",ro)

lim <- ro/sqrt(n)
cat(lim)
cat("limit = 1/ sqrt(pi)")


