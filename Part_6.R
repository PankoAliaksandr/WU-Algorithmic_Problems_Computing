# 126 Please find in separate folder: Rnw file (latex)

# 127. Find reciprocal using Newton's method

task_127_func <- function(y, x0, epsilon)
{
  x_old <- 0.1
  x_new <- 2 * x_old - y * x_old ^ 2
  
  while (abs(x_new - x_old) >= epsilon)
  {
    x_old <- x_new
    x_new <- 2 * x_old - y * x_old ^ 2
  }
  
  return(x_new)
}

# Test
num = 1/6
x0 = 0.1
epsilon = 1e-10

task_127_func(num,x0,epsilon)



# 130.
# Package "rootSolve" must be installed
library(rootSolve)

task_130_func <- function() {
  # Original Function
  f <- function(x)
  {
    return(sin(10 * x) - x)
  }
  
  # Plot function
  x <- seq(-1.5, 1.5, 0.01)
  plot(x, f(x))
  abline(h = 0)
  
  # Find all roots
  roots <- uniroot.all(f, c(-1.5, 1.5))
  
  return(roots)
}


# Test
task_130_func()


# Task 131

# Function
f <- function(x) (((x - 0.5) + x) - 0.5) + x

# If f(x) = 0 possible ?
# It if obvious that f(x) = 3x - 1 => x = 1/3 = 0.(3)
# The best possible accuracy on our PC is 16 digits after comma
# To make f(x) = 0 we need infinite number of digits (because of period)
# So, there is no floating-point number such that f(x) = 0

# Root finder:
# The root returned by uniroot is:
# "0.333333333333333315"
# the value of the function at this argument is:
# "-0.000000000000000056" which is approximately -5.6e-17

# Let's make it not obvious for optimizer
zero <- uniroot(f, lower = -100, upper = 100, tol = 1e-10 )$root
sprintf("%.18f",zero)
f(zero) # close but it is possible to make it better

zero2 <- uniroot(f, lower = -100, upper = 100, tol = .Machine$double.eps, maxiter = 1000, trace = 0)$root
sprintf("%.18f",zero2)
f(zero2) # the best possible accuracy

# Let's check it by increasing tolerance:
zero2 <- uniroot(f, lower = -100, upper = 100, tol = .Machine$double.eps^2, maxiter = 10000, trace = 0)$root
sprintf("%.18f",zero2)
f(zero2) # nothing new

# It is obvious that if tolerance higher result becomes less accurate
# However, tolerance cannot be lower then Machine.eps (1/2^52 = 2.220446e-16)
# which means that using tolerance level lower does not increate the accuracy


# Task 132
# For explanations, please see .pdd 'task_132' 

f <- function(x) x ^ 5 - x ^ 3 - 4 * x
curve(
  expr = f,
  from = -2,
  to = 2,
  main = 'Function Plot',
  xlab = 'argument x',
  ylab = 'F(x)',
  col = "red",
  ylim = c(-10,10)
)

fprime <- function(x) 5 * x ^ 4 - 3 * x ^ 2 - 4

newton1 <- function(x, f, fprime) {
  
  cat("The first 5 members of the \"root\" sequence in Newton's method:", '\n')
  
  for (n in 1:5) {
    x <- x - f(x) / fprime(x)
    cat('x[', n, ']','=',x, '\n')
  }
  
}

# Test
x0 <- 1
newton1(x0, f, fprime)


# Task 133
# Find a minimizer of the function

f <- function(x) (x - 3) ^ 4 + 7 * (x - 2) ^ 2 + x
curve(expr = f, from = 0,to = 5, ylim = c(0,20))

# Solution 1
optim(18, f, gr = NULL, method = c("BFGS"))$value
# Solution 2
nlm(f,1)$minimum


# Task 134
# Explanations are described in  pdf-file 'task_134'

f <- function(x) (5 * x - 3) / (x - 1)
fprime <- function(x) - 2 / (x - 1) ^ 2
curve(expr = f, from = -5,to = 5, ylim = c(-10,10))
newton1 <- function(x, f, fprime) {
  
  cat("The first 5 members of the \"root\" sequence in Newton's method:", '\n')
  
  for (n in 1:5) {
    x <- x - f(x) / fprime(x)
    cat('x[', n, ']','=',x, '\n')
  }
  
}

# Test
newton1(0.5, f, fprime)
newton1(0.75, f, fprime)
newton1(0.2, f, fprime)
newton1(1.24, f, fprime)



# 135
# Explanations: pdf-file name: 'task_135'

f <- function(x) (x ^ 2 - 6 * x + 9) * exp(-x)
curve(expr = f, from = 0,to = 5, ylim = c(-0.05,0.05))
abline( h = 0)
fprime <- function(x) (-x ^ 2 + 8 * x - 15) * exp(-x)

newton1 <- function(x, f, fprime) {
  n = 1
  while (f(x) != 0) {
    x <- x - f(x) / fprime(x)
    n <- n + 1
    cat('Member',n,'is: x=',sprintf("%.16f",x),'f(x)=',f(x),'and fprime(x)=',fprime(x),'\n')
  }
  
}


# Test
newton1(3, f, fprime)
newton1(3.2, f, fprime)
newton1(2.99, f, fprime)
newton1(3.01, f, fprime)


# Task 138
# half-interval method

f <- function(x) sin(x)
curve(expr = f, from = 2,to = 4, ylim = c(-1,1))
abline( h = 0)

half_int <- function(a, b, f, tol) {
  
  # Compute function values at a and b.
  f_a <- f(a)
  f_b <- f(b)
  
  # Initial check  
  if( (f_a > 0 && f_b > 0 ) | (f_a < 0 && f_b < 0 )){
    cat("within provided interval function does ont have zeros")
    return()
  }
  
  n <- 0
  # iterate while the length of interval containing a root is larger
  # than a given tolerance tol
  while (abs(a - b) > tol) {
    midpoint <- (a + b) / 2
    f_mp <- f(midpoint)
    
    if (f_mp == 0) {
      # midpoint is the exact root, stop process
      root <- midpoint 
      return(root)
    }
    
    if (((f_a < 0) & (f_mp < 0)) | ((f_a > 0) & (f_mp > 0))) {
      a <- midpoint
      f_a <- f(a)
    }
    
    else if (((f_b < 0) & (f_mp < 0)) | ((f_b > 0) & (f_mp > 0))) {
      b <- midpoint
      f_b <- f(b)
    }
    
    n <- n + 1
    
  }
  
  # Take the best approximation
  if (abs(f_a) < abs(f_b)) {
    root <- a
  }
  else {
    root <- b
  }
  
  cat ('The root is',sprintf("%.16f",root),'with',-log10(tol),'digits of accuracy, find in', n, 'iterations')
  
  
}

# Test
a <- 2
b <- 4
tol <- 1e-15 # degree of accuracy

half_int(a, b, f, tol)

# 139.
func_a_vector <- function(x)
{
  func_vector <- rep(0,3)
  func_vector[1] <- cos(x)-x
  func_vector[2] <- -sin(x) - 1
  func_vector[3] <- -cos(x)
  return(func_vector)
}

func_a <- function(x)
{
  return(cos(x) - x)
}

func_a_find_root <- function(x0)
{
  x <- x0
  n <- 0
  while (abs(func_a(x)) > 1e-6 & n < 1000000)
  {
    func_vector <- func_a_vector(x)
    A <- 0.5*func_vector[3]
    B <- func_vector[2]-x0*func_vector[3]
    C <- func_vector[1]-x0*func_vector[2]+0.5*x0^2*func_vector[3]
    
    # Calculate Discriminant
    D <- B^2 - 4*A*C
    if (D < 0)
    {
      x <- -B/2/A
    }
    else
    {
      x <- (-B + sqrt(D))/2/A
      
    }
    n <- n+1
    print(n)
  }
  return(x)
}

func_a_find_root(1) # converges to the correct root 0.7390853 in less than 1 mln iterations
func_a_find_root(3) # does not converge to the root in less than 1 mln iterations
func_a_find_root(6) # does not converge to the root in less than 1 mln iterations


<- function(x)
{
  func_vector <- rep(0,3)
  func_vector[1] <- x^3 - x - 3
  func_vector[2] <- 3*x^2 - 1
  func_vector[3] <- 6*x
  return(func_vector)
}

func_b <- function(x)
{
  return(x^3 - x - 3)
}

func_b_find_root <- function(x0)
{
  x <- x0
  n <- 0
  while (abs(func_b(x)) > 1e-6 & n < 1000000)
  {
    func_vector <- func_b_vector(x)
    A <- 0.5*func_vector[3]
    B <- func_vector[2]-x0*func_vector[3]
    C <- func_vector[1]-x0*func_vector[2]+0.5*x0^2*func_vector[3]
    
    # Calculate Discriminant
    D <- B^2 - 4*A*C
    if (D < 0)
    {
      x <- -B/2/A
    }
    else
    {
      x <- (-B + sqrt(D))/2/A
      
    }
    n <- n+1
    print(n)
  }
  return(x)
}

func_b_find_root(0) # converges to the correct root 1 in less than 1 mln iterations



func_c_vector <- function(x)
{
  func_vector <- rep(0,3)
  func_vector[1] <- x^3 - 7*x^2 + 14*x - 8
  func_vector[2] <- 3*x^2 - 14*x + 14
  func_vector[3] <- 6*x - 14
  return(func_vector)
}

func_c <- function(x)
{
  return(x^3 - 7*x^2 + 14*x - 8)
}

func_c_find_root <- function(x0)
{
  x <- x0
  n <- 0
  while (abs(func_c(x)) > 1e-6 & n < 1000000)
  {
    func_vector <- func_c_vector(x)
    A <- 0.5*func_vector[3]
    B <- func_vector[2]-x0*func_vector[3]
    C <- func_vector[1]-x0*func_vector[2]+0.5*x0^2*func_vector[3]
    
    # Calculate Discriminant
    D <- B^2 - 4*A*C
    if (D < 0)
    {
      x <- -B/2/A
    }
    else
    {
      x <- (-B + sqrt(D))/2/A
      
    }
    n <- n+1
    print(n)
  }
  return(x)
}

func_c_find_root(1.1) # converges to the correct root 1 in less than 1 mln iterations
func_c_find_root(1.2) # converges to the correct root 1 in less than 1 mln iterations
func_c_find_root(1.3) # does not converge to the root in less than 1 mln iterations
func_c_find_root(1.4) # converges to the correct root 4 in less than 1 mln iterations
func_c_find_root(1.5) # converges to the correct root 1 in less than 1 mln iterations
func_c_find_root(1.6) # does not converge to the root in less than 1 mln iterations
func_c_find_root(1.7) # does not converge to the root in less than 1 mln iterations
func_c_find_root(1.8) # does not converge to the root in less than 1 mln iterations
func_c_find_root(1.9) # does not converge to the root in less than 1 mln iterations

# 140
# a Newton algorithm sin x near 3
newton <- function(x = 3, f, fprime, epsilon = 1e-14) {
  n <- 0
  
  while(abs(f(x)) > epsilon ) {
    x <- x - f(x) / fprime(x)
    n <- n + 1
  }
  
  cat ('The zero is',sprintf("%.7f",x),'with',-log10(epsilon),
       'digits of accuracy, find in', n, 'iterations')
  return(x)
}
f <- function(x) sin(x)
fprime <- function(x) cos(x)
newton(3,f,fprime)
# b
task_140_func <- function(x,n)
{
  sum <- 0
  for(i in 1:(n+1))
  {
    k <- i - 1
    sum <- sum + ((-1)^k)*(x^(2*k+1))/factorial(2*k+1)
  }
}




# Task 141.
# Package "deconstructSigs" must be installed
library(deconstructSigs)

task_141_func_1 <- function(x)
{
  return(abs(x - 3.5) + abs(x - 2) + abs(x - 1))
}

task_141_func_2 <- function(x)
{
  return(abs(x - 3.2) + abs(x - 3.5) + abs(x - 2) + abs(x - 1))
}

# Plot f1:
x_step_0.01 <- seq(0, 5, 0.01)
plot(x_step_0.01, task_141_func_1(x_step_0.01))

# Minimun f1 is obviously between [-10;10]
golden.section.search(f = task_141_func_1,-10,+10, 1e-06)

# Plot f2:
x_step_0.01 <- seq(1.5, 4, 0.01)
plot(x_step_0.01, task_141_func_2(x_step_0.01))

# The function reaches the minimum of 3.7 when x is in the interval [2,3.2]
# Depending on the input interval the function returns different x
golden.section.search(f = task_141_func_2,-1000,+1000, 1e-06)
golden.section.search(f = task_141_func_1,-5, 2.5, 1e-06)
golden.section.search(f = task_141_func_1, 3.2, 10, 1e-06)
golden.section.search(f = task_141_func_1, 3, 3.1, 1e-06)



# Task 142.

coeff_1 <- c(3, 7, 9, 12, 15)
coeff_2 <- c(3, 7, 9, 12, 15, 18, 21)
coeff_3 <- c(3, 7, 9, 12, 15, 18)

# Check function 1:
task_142_func_1 <- function(x)
{
  sum <- 0
  for ( i in 1:length(coeff_1))
  {
    sum <- sum + abs(x-coeff_1[i])
  }
  return (sum)
  
}

# Check function 2:
task_142_func_2 <- function(x)
{
  sum <- 0
  for ( i in 1:length(coeff_2))
  {
    sum <- sum + abs(x-coeff_2[i])
  }
  return(sum)
}

# Chekc function 3 (even case):
task_142_func_3 <- function(x)
{
  sum <- 0
  for ( i in 1:length(coeff_3))
  {
    sum <- sum + abs(x-coeff_3[i])
  }
  return(sum)
}

# Prove that minimizer is median

# Check 1:
minimizer <- optimize(task_142_func_1,c(min(coeff_1), max(coeff_1)), maximum = FALSE)$minimum
cat("\nminimizer = median?", abs(minimizer - median(coeff_1))< 1e-10)

# Check 2:
minimizer <- optimize(task_142_func_2,c(min(coeff_2), max(coeff_2)), maximum = FALSE)$minimum
cat("\nminimizer = median?", abs(minimizer - median(coeff_2))< 1e-14)

# Check 3(even case):
minimizer <- optimize(task_142_func_2, c(min(coeff_3), max(coeff_3)), maximum = FALSE, tol = 1e-10)$minimum
# Prove that minimizer is in "median" interval
up_bound <- min(which(coeff_3 > median(coeff_3)))
low_bound <- max(which(coeff_3 < median(coeff_3)))
interval <- c(coeff_3[low_bound], coeff_3[up_bound])
cat("\nminimizer is in median interval:",
    (abs(minimizer - interval[1]) > 1e-7 & abs(minimizer - interval[2]) < 1e-7))

# When number of observations is even (x1,....,xn),
# the minimal value of the function will be when all x cancel out.
# This is when minimizer is in "median interval" (interval between 2 median values)

x <- seq(8,13,0.1)
plot(x,task_142_func_3(x))


# Task 144
# Use the golden-section search algorithm to find all local minima of the function

library(deconstructSigs)

f144 <- function(x){
  ifelse(x == 0, 0, abs(x) * log10(abs(x) / 2) * exp(-abs(x))) 
}

# Plot a function:
curve(
  expr = f144,
  from = -10,
  to = 10,
  main = 'Function Plot',
  xlab = 'argument x',
  ylab = 'F(x)',
  col = "red"
)

# Manually
golden <- function (f, a, b, tol = 0.0000001){
  ratio <- 2 / (sqrt(5) + 1)
  x1 <- b - ratio * (b - a)
  x2 <- a + ratio * (b - a)
  f1 <- f(x1)
  f2 <- f(x2)
  while(abs(b - a) > tol) {
    if (f2 > f1) {
      b <- x2
      x2 <- x1
      f2 <- f1
      x1 <- b - ratio * (b - a)
      f1 <- f(x1)
    } else {
      a <- x1
      x1 <- x2
      f1 <- f2
      x2 <- a + ratio * (b - a)
      f2 <- f(x2)
    }
  }
  return((a + b) / 2)
}

# It is clear that the function has 2 local minina
# Just to use different approaches we solved the task using 
# 3 ways

# Fist minimum: interval [-5,0]
golden.section.search(f = f144,-5,0, 1e-06)
optimise(f = f144,interval = c(-5,0),tol = 1e-10)$minimum
golden(f144, -5, 0)

# Second minimun: interval [0,5]
golden.section.search(f = f144,0,5, 1e-06)
optimise(f = f144,interval = c(0,5),tol = 1e-10)$minimum
golden(f144, 0, 5)


# Task 145.

# fuction (a)
x_step_0.1 <- seq(0, 5, 0.1)
plot(x_step_0.1, task_141_func_1(x_step_0.1))
optimize(f = task_141_func_1,interval = c(0,5))

# function (b)
x_step_0.01 <- seq(1.5, 4, 0.01)
plot(x_step_0.01, task_141_func_2(x_step_0.01))

# The function reaches the minimum of 3.7 when x is in the interval [2,3.2]
# Depending on the input interval the function returns different x
optimize(f = task_141_func_2, c(-1000, 1000))
optimize(f = task_141_func_2, c(-5, 2.5))
optimize(f = task_141_func_2, c(3.1, 10))
optimize(f = task_141_func_2, c(3, 3.1))

# Task 146 

# Function to minimize:
f146 <- function(par) {
  a <- par[1]
  b <- par[2]
  (a - 1) + 3.2/b + 3*log(gamma(a)) + 3*a*log(b)
}

# We have restrictions:
# using gamma(): a must be > 0
# using log() b > 0
# It is important to choose initial guess
# Let's assume that optimize means minimize

# Choose the initial guess:
# gamma(x) - increating, log(x) - increasion => a should be minimal possible [1]
# 1\x - decreasing , log(x) - increasion but slower =>  is not clear
# It is clear that c(1,1) is a goog guess

optim(c(1, 1), f146)$value
nlm(f146, c(1, 1))$minimum


# Taks 148
# The Rosenbrock function

# Function
Rosenbrock <- function(x, y) {
  (1 - x)^2 + 100 * (y - x^2)^2
}


# Contour Plot
require(grDevices) # for colours 

x <- seq(-2, 2, len=100)
y <- seq(-2, 5, len=100)
z <- outer(x, y, Rosenbrock)

filled.contour(x, y, z, color = terrain.colors,
               plot.title = title(main = "The Topography of Rosenbrock function",
                                  xlab = "x value", ylab = "y value"),
               plot.axes = { axis(1, seq(-2, 2, by = 1))
                 axis(2, seq(-2, 5, by = 1)) },
               key.title = title(main = "F(x,y)"),
               key.axes = axis(4, seq(0, 5000, by = 300)))

mtext(paste("filled.contour(.) from", R.version.string),
      side = 1, line = 4, adj = 1, cex = 0.5)

# Optimization
Rosenbrock_opt <- function(par) {
  (1 - par[1])^2 + 100 * (par[2] - par[1]^2)^2
}

result <- optim(par = c(0,3), fn = Rosenbrock_opt, method = "L-BFGS-B",
                lower = c(-2,-2),upper = c(2,5))
result$par
result$value



# Task 149
require(grDevices) # for colours

# At first let's have a loop at function:
f149_ <- function(x,y) {
  -(x^2 + y^2 - 2)*(x^2 + y^2 - 1)*(x^2 + y^2 )*(x^2 + y^2 + 1)*(x^2 + y^2 + 2)*
    (2 - sin(x^2 - y^2)*cos(y - exp(y)))
}

x <- seq(-1.5, 1.5, len=100)
y <- seq(-1.5, 1.5, len=100)
z <- outer(x, y, f149_)

filled.contour(x, y, z, color = terrain.colors,
               plot.title = title(main = "The Topography of the function",
                                  xlab = "x value", ylab = "y value"),
               plot.axes = { axis(1, seq(-1.5, 1.5, by = 0.5))
                 axis(2, seq(-1.5, 1.5, by = 0.5)) },
               key.title = title(main = "F(x,y)"),
               key.axes = axis(4, seq(-5000, 3000, by = 1000)))

# Function to find global MINIMUM (sign has been changed)
f149 <- function(par) {
  x <- par[1]
  y <- par[2]
  (x^2 + y^2 - 2)*(x^2 + y^2 - 1)*(x^2 + y^2 )*(x^2 + y^2 + 1)*(x^2 + y^2 + 2)*
    (2 - sin(x^2 - y^2)*cos(y - exp(y)))
}

# Generate random values
random <- runif(10, -1.5, 1.5)

# Find global minimum
f_min <- 10000
arg_min <- c(-100,-100)

for(i in 2:length(random)){
  # take a pair
  init_guess <- random[(i-1):i]
  # try find loc min = loc max of original function
  ans <- optim(par = init_guess, fn = f149, method = "L-BFGS-B",
               lower = c(-1.5,-1.5),upper = c(1.5,1.5))
  # update minimum
  if(f_min > ans$value){
    arg_min <- ans$par
    f_min <- ans$value
  }
}

# Now we have found global minimum of changed function
# Find original f_max and arg_max
- f_min
arg_min

# Now double check
x <- seq(0.6, 0.7, len=100)
y <- seq(-1.1, -1, len=100)
z <- outer(x, y, f149_)

filled.contour(x, y, z, color = terrain.colors,
               plot.title = title(main = "The Topography of the function",
                                  xlab = "x value", ylab = "y value"),
               plot.axes = { axis(1, seq(0.6, 0.7, by = 0.1))
                 axis(2, seq(-1.1, -1, by = 0.1)) },
               key.title = title(main = "F(x,y)"),
               key.axes = axis(4, seq(0, 10, by = 1)))

# Seems like being truth :)


# Task 153
# Original
task_153_func <- function(x, y)
{
  return(2*x^3 - 3*x^2 - 6*x*y*(x-y-1))
}

# Original with 1 parameter-vector
task_153_func_to_min <- function (p)
{
  return(2*p[1]^3 - 3*p[1]^2 -6*p[1]*p[2]*(p[1]-p[2]-1))
}

# Negation
task_153_func_to_max <- function (p)
{
  return(-task_153_func_to_min(p))
}


# Plot contour
x <- seq(-2, 2, by = 0.1)
y <- seq(-2, 2, by = 0.1)
z = outer(x,y,task_153_func)

filled.contour( x, y, z, color = terrain.colors,
                plot.title = title(xlab = "x value", ylab = "y value"),
                plot.axes = { axis(1, seq(-2, 2, by = 0.5))
                  axis(2, seq(-2, 2, by = 0.5)) },
                key.axes = axis(4, seq(min(z), max(z), by = 20)))

# Find minimum 
optim(f = task_153_func_to_min, par = c(1,1))  # converges to a local minimum in the point (1,0)
optim(f = task_153_func_to_min, par = c(5,0))  # converges to a local minimum in the point (1,0)
optim(f = task_153_func_to_min, par = c(-5,5)) # attempt to find global min

# Find maximum 
optim(f = task_153_func_to_max, par = c(-2,-2))$par  # converges to a local maximum in the point (-1,-1)
optim(f = task_153_func_to_max, par = c(-3,-3))$par  # converges to a local maximum in the point (-1,-1)
optim(f = task_153_func_to_max, par = c(2,-2))$par   # does not converge to a local maximum in the point (-1,-1)
-optim(f = task_153_func_to_max, par = c(-2,-2))$value  # converges to a local maximum in the point (-1,-1)
-optim(f = task_153_func_to_max, par = c(-3,-3))$value  # converges to a local maximum in the point (-1,-1)
-optim(f = task_153_func_to_max, par = c(2,-2))$value   # does not converge to a local maximum in the point (-1,-1)

# Overall, it is clear the function doesn't have global max and min


# Task 154
# See also a photo with calculations
# Original
task_154_func <- function(x, y)
{
  return(2*x^2 - 1.05*x^4 + x^6/6 + x*y + y^2)
}

# Original with 1 parameter
task_154_func_to_min <- function (p)
{
  return(2*p[1]^2 - 1.05*p[1]^4 + p[1]^6/6 + p[1]*p[2] +p[2]^2)
}

# Contour plot
# With respect of roots [-2,2] - ok
x <- seq(-2, 2, by = 0.1)
y <- seq(-2, 2, by = 0.1)
z = outer(x,y,task_154_func)

filled.contour( x, y, z, color = terrain.colors,
                plot.title = title(xlab = "x value", ylab = "y value"),
                plot.axes = { axis(1, seq(-2, 2, by = 0.5))
                  axis(2, seq(-2, 2, by = 0.5)) },
                key.axes = axis(4, seq(min(z), max(z), by = 1)))

# There are 5 critical points (3 loc min and 2 sattle ones)
# See explanations
# Find minimum
# As we can see there are 3 local minimums:
optim(f = task_154_func_to_min, par = c(-0.1, 0.1))  # converges to a local minimum in the point (0,0)
optim(f = task_154_func_to_min, par = c(1,1))        # converges to a local minimum in the point (1.75,-0.875)
optim(f = task_154_func_to_min, par = c(-2,2))       # converges to a local minimum in the point (-1.75,0.875)

# The global minimul of the function is 0
