f = open("adjs.txt")
lines = [l.rstrip() for l in f.readlines()]
f.close()

adjs = []
classes = []

for l in lines:
	splited = l.split()
	adjs.append(splited[0][1:-1])
	classes.append(splited[1][1:-1])
	
print adjs

print classes	