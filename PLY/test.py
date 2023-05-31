# import numpy as np

# # Create a sample 2D array
# array_2d = np.array([[1, 2, 3, 3, 4], [4, 5, 6, 5, 6], [7, 8, 9, 6, 5], [7, 8, 9, 6, 5]])

# # Specify the position to check
# row = 2
# col = 2

# # Get the value at the specified position
# value = array_2d[row, col]

# print(array_2d)

# # Check if the position has neighbors of the same value
# has_same_neighbors = np.any(array_2d[row-1:row+2, col-1:col+2] == value)

# #print(array_2d[row-1:row+2, col-1:col+2])
# print(array_2d[row-1][col-1],array_2d[row-1][col],array_2d[row-1][col+1])
# print(array_2d[row][col-1]," ",array_2d[row][col+1])
# print(array_2d[row+1][col-1],array_2d[row+1][col],array_2d[row+1][col+1])
# #print(array_2d[row-1][col-1],array_2d[row-1][col],array_2d[row][col+1])
# #Print the result
# print(has_same_neighbors)


# #factorial
# iterations= 0
# factorial = 3
# start = 1
# while (iterations<factorial):
#     iterations += 1
#     start *= iterations

# #print(start)

# #fibonachi
iterations= 0
times = 5
start = 1
result  = 1
while (iterations<times):
    iterations += 1
    aux = result
    result = start + result
    start = aux

print(result)

# array = [1,2,3,4,5,6,7]

# print(array[3:])
