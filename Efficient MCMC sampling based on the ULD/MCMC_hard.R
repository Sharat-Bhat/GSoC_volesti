library(volesti)
library(ggplot2)
library(geometry)

num_dim = 150
# step_size = 1
error = 0.001

# weight_matrix = matrix(, nrow = num_dim, ncol = 3)
weight_matrix_vector = vector(mode = "numeric", length = 2*num_dim+1)
weight_matrix_gradient_vector = vector(mode = "numeric", length = 2*num_dim)

strong_convex_create <- function()
{
	weight_matrix_vector[1] <<- 50*runif(1)+50
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
	extremum = vector(mode="numeric", length = num_dim)
}

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

bb_gradient_descent <- function()
{
	previous_vector = rep(1, num_dim)
	previous_vector_matrix = matrix(previous_vector, nrow = 1)
	init_step = 0.001
	current_vector_matrix = previous_vector_matrix - init_step*strong_convex_gradient(previous_vector)
	flag = TRUE
	current_vector = as.vector(current_vector_matrix)
	previous_vector = as.vector(previous_vector_matrix)
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
maxima = bb_gradient_descent()
cat("Maxima = ", maxima, "\n\n")