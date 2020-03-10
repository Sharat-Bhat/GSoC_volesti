library(volesti)
library(ggplot2)
library(geometry)

num_dim = 100
error = 0.01
step_size = 1
num_points = 400

distance <- function(point1, point2)
{
	ans = 0
	for(i in 1:num_dim)
	{
		ans = ans + (point1[i] - point2[i])**2
	}
	ans = sqrt(ans)
	return(ans)
}

strong_convex <- function(point_vector)
{
	ans = 0
	for (coordinate in point_vector)
	{
		ans = ans + (coordinate)**2/20
	}
	ans = exp(ans) + 4
	return(ans)
}

prob_density <- function(point_vector)
{
	prob_dens = exp(-strong_convex(point_vector))
	return (prob_dens)
}

component_of_walk <- function(cumu_freq, value)
{
	n = length(cumu_freq)
	begin = 1
	end = n
	mid = as.integer((begin+end)/2)
	while ((cumu_freq[mid] < value || cumu_freq[mid-1] > value) && begin <= end)
	{
		if(cumu_freq[mid] < value)
		{
			begin = mid + 1
		}
		else if(cumu_freq[mid-1] > value)
		{
			end = mid - 1
		}
		mid = as.integer((begin+end)/2)
	}
	return(2*mid/n -1)
}

random_point <- function(start_point)
{
	n = num_dim - 1
	num_parts = as.integer(1/error)
	cumu_freq = vector(mode="numeric", length = 2*num_parts)
	radius = step_size
	for (j in 1:(num_dim-2))
	{
		cumu = 0
		for(i in 1:num_parts)
		{
			temp = start_point
			r = as.numeric((num_parts - i)/num_parts)
			index = (n-1)/2
			temp[j] = temp[j] + r
			delta_freq = error*((1-r*r)**index)*prob_density(temp)
			cumu = cumu + delta_freq
			cumu_freq[i] = cumu		
		}
		for(i in 1:num_parts)
		{
			temp = start_point
			r = as.numeric(i/num_parts)
			index = (n-1)/2
			temp[j] = temp[j] + r
			delta_freq = error*((1-r*r)**index)*prob_density(temp)
			cumu = cumu + delta_freq
			cumu_freq[num_parts + i] = cumu
		}
		random = runif(1) * cumu
		x = radius*component_of_walk(cumu_freq, random)
		start_point[j] = start_point[j] + x
		radius = (radius**2 - x**2)**0.5
	}
	theta = 2*pi*runif(1)
	start_point[num_dim-1] = start_point[num_dim-1] + radius*cos(theta)
	start_point[num_dim] = start_point[num_dim] + radius*sin(theta)
	return(start_point)
}

ball_walk <-function(start_point)
{
	random_walk_points = vector("list", num_points)
	random_walk_points[[1]] = start_point
	for(i in 2:num_points)
	{
		next_point = random_point(start_point)
		# cat(next_point, "\n")
		random_walk_points[[i]] = next_point
		start_point = next_point
	}
	return(random_walk_points)
}

# Generating a random vector as Starting Point (as of now)
vec = vector("numeric", num_dim)
for(i in 1:num_dim)
{
	vec[i] = runif(1)
}

random_walk_points = ball_walk(vec)
displacement = distance(random_walk_points[[1]], random_walk_points[[num_points]])
cat("Random Walk: \n")
for(point in random_walk_points)
{
	cat(point,"\n")
}

i1 = as.integer(num_dim*runif(1)) + 1
i2 = as.integer(num_dim*runif(1)) + 1
while(i2 == i1)
{
	i2 = as.integer(num_dim*runif(1))+1
}
cat(i1, i2, "\n")
x = vector("numeric", num_points)
for(i in 1:num_points)
{
	x[i] = random_walk_points[[i]][i1]
}
y = vector("numeric", num_points)
for(i in 1:num_points)
{
	y[i] = random_walk_points[[i]][i2]
}

# Plotting them in a graph
heading = paste("Random Ball Walk\nStep Size = ", step_size," Number of Dimensions = ", num_dim, "Number of Points = ", num_points)
cat("Displacement = ", displacement, "\n")
plot(x, y, type = "n", main = heading)
lines(x, y, type = "o")