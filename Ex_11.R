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


