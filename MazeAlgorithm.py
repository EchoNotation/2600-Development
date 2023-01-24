import random

n = 8 #Width and height of maze in tiles

maze = []

#Maze tiles represented as NSEW where a 0 indicates no passage possible, and a 1 indicates passage possible

#Generate maze
for y in range(n):
	maze.append([])
	for x in range(n):
		#print(f"{y}, {x}")
		north = ""
		south = ""
		east = ""
		west = ""
		exitCounter = 0

		if(y == 0):
			north = "0"
		elif(maze[y-1][x][1] == "1"):
			north = "1"
		else:
			north = "0"

		if(x == 0):
			west = "0"
		elif(maze[y][x-1][2] == "1"):
			west = "1"
		else:
			west = "0"

		if(y == n-1):
			south = "0"
		else:
			south = random.choice(["0","1"])
			exitCounter += int(south)
		if(x == n-1):
			east = "0"
		else:
			east = random.choice(["0","1"])
			exitCounter += int(east)

		if(exitCounter == 0 and not(x == n-1 and y == n-1)):
			#There were no created exits from this cell, create some!
			if(x == n-1):
				south = "1"
			elif(y == n-1):
				east = "1"
			else:
				if(random.choice(['S','E']) == 'S'):
					south = "1"
				else:
					east = "1"

		tile = north + south + east + west
		maze[y].append(tile)

dictionary = {"0000":"X", "0001":"<", "0010":">", "0011":"<>", "0100":"v", "0101":"<v", "0110":"v>", "0111":"<v>", "1000":"^", "1001":"<^", "1010":"^>", "1011":"<^>", "1100":"|", "1101":"<|", "1110":"|>", "1111":"+"}
usefulDictionary = {"0000":"0", "0001":"1", "0010":"2", "0011":"3", "0100":"4", "0101":"5", "0110":"6", "0111":"7", "1000":"8", "1001":"9", "1010":"A", "1011":"B", "1100":"C", "1101":"D", "1110":"E", "1111":"F"}
#Convert maze to better visual representation
for i in range(n):
	for j in range(n):
		maze[i][j] = dictionary[maze[i][j]]

#Print out maze
for i in range(n):
	print(maze[i])