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

