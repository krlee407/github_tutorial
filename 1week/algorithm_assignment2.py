# matrix는 2차원 list
def Largest(matrix):
	height = len(matrix)
	width = len(matrix[0])

	dp = [[0 for _ in range(width)] for _ in range(height)]

	for y in range(height):
		for x in range(width):
			if y <= 0 or x <= 0:
				continue

			if matrix[y][x]==1 and matrix[y-1][x]==1 and \
			matrix[y][x-1]==1 and matrix[y-1][x-1]==1:
				dp[y][x] = dp[y-1][x-1]+1

	result = 0
	for y in range(height):
		for x in range(width):
			if result < dp[y][x]:
				result = dp[y][x]

	return (result+1)**2

print(Largest([[1,0,1,1,1],[0,0,0,1,1],[0,1,1,1,1],[0,1,1,1,1],[0,1,1,1,1]]))
print(Largest([[1,0,1,1,1],[1,1,1,1,1],[0,1,1,1,1],[0,1,1,1,1],[0,1,1,1,1]]))
