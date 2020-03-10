import random
import math
import matplotlib.pyplot as plt

num_dim = 3 #Number of dimensions
epsilon = 0.1
step_size = 1
num_steps = 10
"""
This is the black box function which returns
the value on passing a parameter, i.e, point vector
"""
def strong_convex(point_vector):
	n = len(point_vector)
	ans = 0
	for coordinate in point_vector:
		ans += coordinate**2/2
	return ans

def prob_density(point_vector):
	return math.exp(-strong_convex(point_vector))

def find_interval(cumu_freq, prob):
	print("dfnn")
	n = len(cumu_freq)
	for i in range(n):
		if cumu_freq[i] > prob:
			print("Interval is ", i)
			return i
	# if cumu_freq[0] > prob:
	# 	return 0
	# high = n-1
	# low = 0
	# mid = int((high+low)/2)
	# while not (cumu_freq[mid] > prob and cumu_freq[mid-1] < prob):
	# 	if cumu_freq[mid] < prob:
	# 		low = mid + 1
	# 	elif cumu_freq[mid-1] > prob:
	# 		high = mid - 1
	# 	mid = int((high+low)/2)
	# print("Interval is ", mid)
	# return mid

def random_point(start_point):
	print("bfffffffffff")
	global epsilon, num_dim
	# ball_walk = [start_point]
	num_parts = int(1/epsilon)
	cumu = 0
	global step_size
	ans = []
	radius = step_size
	temp_point = start_point
	n = num_dim - 1
	for j in range(num_dim-1):
		cumu_freq = []
		for i in range(num_parts+1):
			r = 2*(i)/num_parts-1
			# temp_point = start_point
			temp_point[j] += 2/num_parts
			delta_freq = epsilon*((1-r*r)**((n-1)/2))*prob_density(temp_point)
			print(r, delta_freq)
			cumu += delta_freq
			cumu_freq.append(cumu)
		cumu_freq = [freq/cumu for freq in cumu_freq]
		choice = random.uniform(0, 1)
		print(cumu_freq)
		x = (2*find_interval(cumu_freq, choice)/num_parts - 1)*radius
		print(choice, radius, x)
		ans.append(temp_point[j] + x)
		start_point[j] += x
		radius = math.sqrt(radius**2-x**2)
		n -= 1
		# temp_point.pop(0)
	ans.append(temp_point[num_dim-1] + radius)
	return ans

def ball_walk(start_point):
	global num_steps
	ball_walk = [start_point]
	print("Start point is ", start_point)
	for i in range(num_steps):
		next_point = random_point(start_point)
		print("Point",i,"generated is ", next_point)
		ball_walk.append(next_point)
		start_point = next_point
	return ball_walk

if __name__ == "__main__":
	start = [0, 0, 0]
	steps = ball_walk(start)
	x_walk = [point[0] for point in steps]
	y_walk = [point[1] for point in steps]
	plt.scatter(x_walk, y_walk, label = "circles", color = "green", marker = "o")
	plt.plot(x_walk, y_walk, label = "Ball Walk")
	plt.xlabel("X-Axis")
	plt.ylabel("Y-Axis")
	plt.title("Ball Walk in n-dimensions")
	plt.legend()
	plt.show()


