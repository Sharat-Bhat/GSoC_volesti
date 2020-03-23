library(volesti)
library(ggplot2)
library(geometry)

# Number of dimensions of the space
num_dim = 4
step_size = 0.1
num_points = 100000
num_samples = 10000

# Vector containing the corresponding coefficients of the evaluate function
weight_matrix_vector = vector(mode = "numeric", length = 2*num_dim+1)

# Coordinate Axes
p = num_dim-2
q = num_dim

distance <- function(point1, point2)
{
	diff = point2 - point1
	ans = sum(diff*diff)
	ans = sqrt(ans)
	return(ans)
}

# For Testing Purpose only
# Randomly creates a strong convex function
strong_convex_create <- function()
{
	weight_matrix_vector[1] <<- 100*runif(1)+100

	# Extremum lies inside the the cube of edge length 2
	for(i in 1:num_dim)
	{
		weight_matrix_vector[i+1] <<- 200*runif(1) - 100
	}
	for(i in 1:num_dim)
	{
		weight_matrix_vector[i+num_dim+1] <<- 10*runif(1) + 5
	}
	# weight_matrix_vector[3] <<- 0
	# weight_matrix_vector[num_dim+3] <<- 0
	# weight_matrix_vector[2] <<- 0
	# weight_matrix_vector[num_dim+2] <<- 0
	cat("Weight_matrix_vector: \n", weight_matrix_vector, "\n\n")
	cat("\nMean = ",-weight_matrix_vector[p+1]/(2*weight_matrix_vector[num_dim+p+1]), -weight_matrix_vector[q+1]/(2*weight_matrix_vector[num_dim+q+1]), "\n")
	cat("Standard Deviation = ", 1/sqrt(2*weight_matrix_vector[num_dim+p+1]), 1/sqrt(2*weight_matrix_vector[num_dim+q+1]),"\n")
}

# Evaluates the value of the function at a particular point
strong_convex_evaluate <- function(point_vector)
{
	ans = 0
	point_matrix_vector = vector(mode = "numeric", length = 2*num_dim+1)
	point_matrix_vector[1] = 1
	for(i in 1:num_dim)
	{
		point_matrix_vector[i+1] = point_vector[i]
	}
	for(i in 1:num_dim)
	{
		point_matrix_vector[i+1+num_dim] = point_vector[i]**2
	}
	for(i in 1:2*num_dim+1)
	{
		ans = ans + point_matrix_vector[i]*weight_matrix_vector[i]
	}
	ans = ans / 10
	return(ans)
}

# Marsaglia Method used followed by MCMC Algorithm for Inhomogenous Distribution
random_point <- function(start_point)
{
	next_step = vector(mode="numeric", length = num_dim)
	next_point = vector(mode="numeric", length = num_dim)
	origin = rep(0, num_dim)
	next_step = rnorm(num_dim)
	next_step = step_size / distance(origin, next_step) * next_step
	# cat(next_step, "\n")
	next_point = start_point + next_step
	transition = exp(-strong_convex_evaluate(next_point)+strong_convex_evaluate(start_point))
	prob = runif(1)
	# cat(prob, transition, "\n\n")
	if (transition > prob)
		return(next_point)
	else
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
vec = 10*runif(n = num_dim)
vec = rep(-10, num_dim)
strong_convex_create()
cat("Start = ", vec[p], vec[q], "\n")
random_walk_points = ball_walk(vec)
displacement = distance(random_walk_points[[1]], random_walk_points[[num_points]])
cat("End = ", random_walk_points[[num_points]][p], random_walk_points[[num_points]][q], "\n")
x = vector("numeric", num_points)
for(i in 1:num_points)
{
	x[i] = random_walk_points[[i]][p]
}
y = vector("numeric", num_points)
for(i in 1:num_points)
{
	y[i] = random_walk_points[[i]][q]
}

# Plotting them in a graph
heading = paste("Random Ball Walk\nStep Size = ", step_size," Number of Dimensions = ", num_dim, "Number of Points = ", num_points)
cat("Displacement = ", displacement, "\n")
plot(x, y, type = "p", main = heading, col = "red", pch =".")
# lines(x, y)
# random_walk_points <- data.frame(random_walk_points)
# ggplot(random_walk_points, aes(x = x, y = y)) + stat_density2d(aes(fill = ..density..), contour = F, geom = 'tile')