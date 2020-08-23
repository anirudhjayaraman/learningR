###############################################################################
###                                                                         ###
### Assignment for the course on Advanced R Programming by JHU (Coursera)   ###
### Author: Anirudh Jayaraman                                               ###
###                                                                         ###
###############################################################################
# =============================================================================
# The objective of Part 1 is to write a function that computes the factorial 
# of an integer greater than or equal to 0. Recall that the factorial of a 
# number n is n * (n-1) * (n - 2) * … * 1. 
# The factorial of 0 is defined to be 1. 

# For this Part you will need to write four different versions of the Factorial 
# function:
#   
# Factorial_loop:   computes the factorial of an integer using looping.
# Factorial_reduce: computes the factorial using the reduce() function in 
#                   the purrr package. Alternatively, you can use the Reduce() 
#                   function in the base package.
# Factorial_func:   uses recursion to compute the factorial.
# Factorial_mem:    uses memoization to compute the factorial.
# 
# After writing your four versions of the Factorial function, use the 
# microbenchmark package to time the operation of these functions and provide a 
# summary of their performance. In addition to timing your functions for 
# specific inputs, make sure to show a range of inputs in order to demonstrate 
# the timing of each function for larger inputs.
# =============================================================================

# Loading libraries relevant to this exercise ---------------------------------

require(tidyverse)
require(microbenchmark)

# =============================================================================
# Different implementations of the factorial functions  
# =============================================================================

# 1. --------------------------------------------------------------------------
Factorial_loop <- function(n){
  #' A function that computes the factorial of an integer using
  #' looping
  #' @param n A non-negative integer

  # stopifnot(n >=0)
  
  result <- 1
  
  if (n <= 1) return(result)
  
  for (i in n:2) {
    result <- result * i
  }
  return(result)
}

# 2. --------------------------------------------------------------------------
Factorial_reduce <- function(n){
  #' A function that computes the factorial of an integer using the reduce
  #' function in the purrr package. 
  #' @param n A non-negative integer
  
  # stopifnot(n >=0)
  
  if (n <= 1) return(1)
  
  return(reduce(.x = n:2, .f = `*`))
}

# 3. --------------------------------------------------------------------------
Factorial_func <- function(n){
  #' A function that uses recursion to compute the factorial
  #' @param n A non-negative integer
  
  # stopifnot(n >=0)
  
  return(if (n <= 1) 1 else n * Factorial_func(n-1))
}

# 4. --------------------------------------------------------------------------
factorial_table <- rep(NA, 100)

Factorial_mem <- function(n){
  #' A function that uses memoization to compute the factorial
  #' @param n A non-negative integer
  
  # stopifnot(n >=0)
  
  if(n==0) 1
  else if(!is.na(factorial_table[n])) return(factorial_table[n])
  else {
    factorial_table[n-1] <<- Factorial_mem(n-1)
    return(n * Factorial_mem(n-1))
  }
}

# =============================================================================
# After writing your four versions of the Factorial function, use the 
# microbenchmark package to time the operation of these functions and provide 
# a summary of their performance. In addition to timing your functions for 
# specific inputs, make sure to show a range of inputs in order to demonstrate 
# the timing of each function for larger inputs.
# =============================================================================

# Test cases ==================================================================

# Single inputs ---------------------------------------------------------------
input_1 <- 0
input_2 <- 25
input_3 <- 7
input_4 <- 62

# Range of inputs -------------------------------------------------------------

# Vector of 15 input test cases
input_range_1 <- c(27, 37, 57, 89, 20, 86, 97, 62, 58, 6, 19, 16, 61, 34, 0)

# Vector of a sequence of the first 100 integers starting at 0
input_range_2 <- seq(0,100,1)

# Defining the benchmarking function ==========================================

benchmark_inputs <- function(x){
  #' apply microbenchmark using the four factorial functions that are 
  #' defined - Factorial_func, Factorial_loop, Factorial_reduce & Factorial_mem
  #' @param x a non-zero positive integer or vector of such integers
  
  stopifnot(if(length(x)==1) x >=0 else sum(x<0) == 0)
  
  cat("\nTest input: ", x, "\n")
  
  return(microbenchmark(x %>% map_dbl(.f = Factorial_func),
                        x %>% map_dbl(.f = Factorial_loop),
                        x %>% map_dbl(.f = Factorial_reduce),
                        x %>% map_dbl(.f = Factorial_mem)))
}

# Results of benchmarking =====================================================
# Single inputs
benchmark_inputs(input_1)
benchmark_inputs(input_2)
benchmark_inputs(input_3)
benchmark_inputs(input_4)

# Range of inputs
benchmark_inputs(input_range_1)
benchmark_inputs(input_range_2)

# Saving results to a text file ===============================================

sink("./advanced_r_programming_jhu/assignment/factorial_output.txt", append = FALSE)

cat("\n# Input Test Case: Single non-negative integer ---------------------\n")

benchmark_inputs(input_1)
benchmark_inputs(input_2)
benchmark_inputs(input_3)
benchmark_inputs(input_4)

cat("\n# Input Test Case: Vector of non-negative integers -----------------\n")

benchmark_inputs(input_range_1)
benchmark_inputs(input_range_2)

cat("\n# That's all folks -------------------------------------------------\n")

sink()

##############################################################################
