from random import randrange

def dice():
	return randrange(6)+1+randrange(6)+1 # 주사위 하나가 1~6까지(randrange(6)+1)

def CRAPS(a=False):
	num = dice()

	if num in [7,11]:
		return 1
	elif num in [2,3,12]:
		return -1+a
	else:
		win_num = num
		while(1):
			num = dice()
			if num == win_num:
				return 1
			elif num == 7:
				return -1+a

iteration = 1000
answer_a = 0
for _ in range(iteration):
	answer_a += CRAPS(True)
answer_a /= iteration

def game_b(KY_coin, SY_coin):
	while(1):
		result_CRAPS = CRAPS()
		KY_coin += result_CRAPS
		SY_coin -= result_CRAPS
		if KY_coin <= 0:
			return 0
		elif SY_coin <= 0:
			return 1

answer_b1 = 0
answer_b2 = 0
for _ in range(iteration):
	answer_b1 += game_b(12,9)
	answer_b2 += game_b(20,9)
answer_b1 /= iteration
answer_b2 /= iteration


print('Answer (a) : {}'.format(answer_a))
print('Answer (b) : {}, {}'.format(answer_b1, answer_b2))