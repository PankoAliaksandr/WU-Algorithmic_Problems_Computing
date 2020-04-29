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

