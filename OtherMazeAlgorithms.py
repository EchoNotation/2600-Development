import random

mazeWidth = 8
vEdges = []
hEdges = []

#random.seed(1)

squareIndex = 0
squares = [8, 7, 7, 6, 6, 5, 5, 4, 4, 3, 3, 2, 2, 1, 1]
squaresRemaining = squares[squareIndex]

for i in range(mazeWidth):
	#vEdges.append([0, 0, 0, 0, 0, 0, 0])
	vEdges.append([1, 1, 1, 1, 1, 1, 1])
for i in range(mazeWidth-1):
	#hEdges.append([0, 0, 0, 0, 0, 0, 0, 0])
	hEdges.append([1, 1, 1, 1, 1, 1, 1, 1])

dir = 0 #0 is East, 1 is South, 2 is West, 3 is North
x = 0
y = 0

count = 0

def printMaze():
	for i in range(mazeWidth):
		if i < mazeWidth-1:
			vString = " "
			for edge in vEdges[i]:
				if edge == 1:
					vString += " | "
				else:
					vString += "   "
			print(vString)
			hString = ""
			for edge in hEdges[i]:
				if edge == 1:
					hString += "-- "
				else:
					hString += "   "
			print(hString)
		else:
			vString = " "
			for edge in vEdges[i]:
				if edge == 1:
					vString += " | "
				else:
					vString += "   "
			print(vString)

while True:
	#print("X: " + str(x) + " Y: " + str(y))
	
	count += 1

	inlineExit = 1
	adjacentExit = 1

	if squaresRemaining == 1:
		adjacentExit = 0

		match dir:
			case 0:
				hEdges[y][x] = adjacentExit
			case 1:
				vEdges[y][x-1] = adjacentExit
			case 2:
				hEdges[y-1][x] = adjacentExit
			case 3:
				vEdges[y][x] = adjacentExit
	else:	
		if random.randint(0, 1) == 1:
			#Make an exit
			inlineExit = 0
		if random.randint(0, 1) == 1:
			#Make another exit
			adjacentExit = 0
		if inlineExit == 1 and adjacentExit == 1:
			if random.randint(0, 1) == 1:
				inlineExit = 0
			else:
				adjacentExit = 0

		match dir:
			case 0:
				vEdges[y][x] = inlineExit
				hEdges[y][x] = adjacentExit
			case 1:
				vEdges[y][x-1] = adjacentExit
				hEdges[y][x] = inlineExit
			case 2:
				vEdges[y][x-1] = inlineExit
				hEdges[y-1][x] = adjacentExit
			case 3:
				vEdges[y][x] = adjacentExit
				hEdges[y-1][x] = inlineExit

	#print("i: " + str(inlineExit) + " a: " + str(adjacentExit))

	squaresRemaining -= 1
	if squaresRemaining == 0:
		dir = (dir+1) % 4
		squareIndex += 1

		if squareIndex >= len(squares): break

		squaresRemaining = squares[squareIndex]

	match dir:
		case 0:
			x += 1
		case 1:
			y += 1
		case 2:
			x -= 1
		case 3:
			y -= 1
	
	#printMaze()
	#input()

printMaze()