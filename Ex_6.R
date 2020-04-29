# Task 6
# Using seq() and rep() as needed, create the vectors
# Sequence 1: 0 0 0 0 0 1 1 1 1 1 2 2 2 2 2 3 3 3 3 3 4 4 4 4 4 and 
# Sequence 2: 1 2 3 4 5 1 2 3 4 5 1 2 3 4 5 1 2 3 4 5 1 2 3 4 5

result_vector <- rep(seq(from = 0,to = 4), each = 5)
cat(" Sequence 1: ", result_vector)
result_vector <- rep(seq(from = 1,to = 5), times = 5)
cat(" Sequence 2: ", result_vector)
