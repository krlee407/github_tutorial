def cofactor(matrix, i, j):
	matrix_copy = [row.copy() for row in matrix]
	matrix_copy.pop(i)
	for i in range(len(matrix_copy)):
		matrix_copy[i].pop(j)
	return matrix_copy

def determinent(matrix):
	if len(matrix)==1:
		return matrix[0][0]
	if len(matrix)==2:
		return matrix[0][0]*matrix[1][1]-matrix[0][1]*matrix[1][0]
	det = 0
	for i in range(len(matrix)):
		det += ((-1)**i)*(matrix[i][0]*determinent(cofactor(matrix,i,0)))
	return det

def inverse(matrix):
	det = determinent(matrix)

	inverse_matrix = [[0 for _ in range(len(matrix[0]))] for _ in range(len(matrix))]

	for i in range(len(matrix)):
		for j in range(len(matrix[0])):
			inverse_matrix[j][i] = ((-1)**(i+j))*determinent(cofactor(matrix,i,j)) / det

	return inverse_matrix

def dot_product(a, b):
	return sum([i*j for i, j in zip(a,b)])

def matrix_multiply(A, b):
	'''
		A : 2차원 matrix
		b : 1차원 vector
	'''
	result = []
	for row in A:
		result.append(dot_product(row, b))
	return result

def solveEquation(A=None,x=None,b=None):
	if x is None:
		return matrix_multiply(inverse(A), b)
	if b is None:
		return matrix_multiply(A, x)
	if matrix_multiply(A, x) == b:
		return 'Correct'
	else:
		return 'Incorrect'

A = [[0,1,2],[3,4,5],[6,7,8]]
B = [[2,2,0],[-2,1,1],[3,0,1]]
C = [[1,2],[3,4]]

print(solveEquation(A, [1,2,3]))
print(solveEquation(B, b=[1,2,3]))
print(solveEquation(C, [1,2], [5,11]))