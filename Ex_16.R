# Task 17
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

