# Task 7
# Using seq() and rep() as needed, create the vectors
# Vector: 1 2 3 4 5 2 3 4 5 6 3 4 5 6 7 4 5 6 7 8 5 6 7 8 9

result_vector <- seq(from = 1,to = 5) + rep(seq(from = 0,to = 4), each = 5)
cat(" Vector: ", result_vector)
