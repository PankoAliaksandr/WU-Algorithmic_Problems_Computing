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

