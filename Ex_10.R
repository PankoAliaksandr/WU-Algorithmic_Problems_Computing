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




