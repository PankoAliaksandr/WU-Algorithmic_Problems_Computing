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


