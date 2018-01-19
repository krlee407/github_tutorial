def chopchop(chop_list):
	length = len(chop_list)
	if length < 3:
		return -1
	dp = [[100000000 for _ in range(length)] for _ in range(length)]

	for j in range(length-1):
		for i in range(length-1):
			if i+j>=length-1:
				continue
			if j==0:
				dp[i][i+j] = 0
			for k in range(i, i+j):
				dp[i][i+j] = min(dp[i][k]+dp[k+1][i+j]+chop_list[i]*chop_list[k+1]*chop_list[i+j+1], dp[i][i+j])

	return dp[0][length-2]

print(chopchop([10,20,5,30,15]))
print(chopchop([10,20,5,30]))