library(volesti)
library(ggplot2)
library(geometry)

# Number of dimensions of the space
num_dim = 200
# Desirable error in the final coordinates of the extremum
error = 0.0001

# Vector containing the corresponding coefficients of the evaluate function
weight_matrix_vector = vector(mode = "numeric", length = 2*num_dim+1)
# Vector containing the corresponding coefficients of the gradient function
weight_matrix_gradient_vector = vector(mode = "numeric", length = 2*num_dim)

# Creates a random strongly convex function
strong_convex_create <- function()
{
	weight_matrix_vector[1] <<- 100000*runif(1)+100000

	# Extremum lies inside the the cube of edge length 200000
	for(i in 1:num_dim)
	{
		weight_matrix_vector[i+1] <<- 200000*runif(1) - 100000
		weight_matrix_gradient_vector[i] <<- weight_matrix_vector[i+1]
	}
	for(i in 1:num_dim)
	{
		weight_matrix_vector[i+num_dim+1] <<- runif(1) + 0.5
		weight_matrix_gradient_vector[i+num_dim] <<- 2*weight_matrix_vector[i+num_dim+1]
	}
	cat("Weight_matrix_vector: \n", weight_matrix_vector, "\n\n")
	cat("Weight_matrix_gradient_vector: \n", weight_matrix_gradient_vector, "\n\n")
	expected_extremum = vector(mode="numeric", length = num_dim)
	for(i in 1:num_dim)
	{
		expected_extremum[i] = -weight_matrix_vector[i+1]/(2*weight_matrix_vector[num_dim+i+1])
	}
	cat("Expected Extremum: \n", expected_extremum,"\n\n")
}

# Evaluates the value of the function at a particular point
strong_convex_evaluate <- function(point_vector)
{
	ans = 0
	point_matrix_vector <- vector(mode = "numeric", length = 2*num_dim+1)
	point_matrix_vector[1] = 1
	for(i in 2:num_dim+1)
	{
		point_matrix_vector[i] = point_vector[i-1]
	}
	for(i in num_dim+1:2*num_dim+1)
	{
		point_matrix_vector[i] = point_vector[i-1-num_dim]**2
	}
	for(i in 1:2*num_dim+1)
	{
		ans = ans + point_matrix_vector[i]*weight_matrix_vector[i]
	}
	return(ans)
}

# Evaluates the gradient vector of the function at a particular point
strong_convex_gradient <- function(point_vector)
{

	point_matrix_vector = vector(mode = "numeric", length = num_dim)
	for(i in 1:num_dim)
	{
		point_matrix_vector[i] = point_vector[i]*weight_matrix_gradient_vector[num_dim+i] + weight_matrix_gradient_vector[i]
	}
	ans = matrix(point_matrix_vector, nrow=1)
	return(ans)
}

# Implements gradient descent using Barzilai-Borwein Method
bb_gradient_descent <- function()
{
	# Initialisation of vectors v0 and v1
	previous_vector = rep(1, num_dim)
	previous_vector_matrix = matrix(previous_vector, nrow = 1)
	init_step = error
	current_vector_matrix = previous_vector_matrix - init_step*strong_convex_gradient(previous_vector)
	flag = TRUE
	current_vector = as.vector(current_vector_matrix)
	previous_vector = as.vector(previous_vector_matrix)
	# Gradient Descent Loop 
	num_steps = 0
	while(flag == TRUE)
	{
		y_vector_matrix = strong_convex_gradient(current_vector) - strong_convex_gradient(previous_vector)
		s_vector_matrix = strong_convex_evaluate(current_vector) - strong_convex_evaluate(previous_vector)
		alpha = as.numeric(s_vector_matrix*t(current_vector - previous_vector))/as.numeric(s_vector_matrix*t(y_vector_matrix))
		previous_vector_matrix = current_vector_matrix
		current_vector_matrix = current_vector_matrix - alpha*strong_convex_gradient(current_vector)
		current_vector = as.vector(current_vector_matrix)
		previous_vector = as.vector(previous_vector_matrix)
		flag = FALSE
		for(i in 1:num_dim)
		{
			if(abs(current_vector[i] - previous_vector[i]) > error)
			{
				flag = TRUE
			}
		}
		num_steps = num_steps+1
	}	
	cat("Number of Steps: ", num_steps,"\n\n")
	return (current_vector)
}

strong_convex_create()
found_extremum = bb_gradient_descent()
cat("Found Extremum: \n", found_extremum, "\n\n")