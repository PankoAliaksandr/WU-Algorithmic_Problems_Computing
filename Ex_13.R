# Task 13
# Write a function which outputs whether a given number exceeds pi in absolute value.

# Check Function
Check_sign <- function(x){
  ifelse(test = ((x > pi) | (x < -pi)), yes = return(TRUE), no = return(FALSE))
}

# Main
check_verctor <- c(-4,5,0)

for(i in 1:length(check_verctor)){
  
  if(Check_sign(check_verctor[i]) == TRUE){
    cat("value",check_verctor[i],"exceeds pi in absolute value\n")
  }
  
  if(Check_sign(check_verctor[i]) == FALSE){
    cat("value",check_verctor[i],"does not exceed pi in absolute value\n")
  }
  
}

