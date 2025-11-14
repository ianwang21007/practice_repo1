# load necessary packages

library(tidyverse)
library(ggplot2)
library(esquisse)

# Task 1: Create the functions

# discrete distribution
discrete_func <- function(size){ # create function
  discrete = sample( # generate the distribution
    x = 1:10, 
    size, 
    replace = TRUE
  )
  difference = median(discrete) - mean(discrete) # find the difference between median and mean for the generated distribution
  return(difference) # return the generated difference
}

# normal distribution
norm_func <- function(n){
  norm = rnorm(
    n, 
    mean = 0, 
    sd = 1
  )
  difference = median(norm) - mean(norm)
  return(difference)
}

# poisson distribution
pois_func <- function(n){
  pois = rpois(
    n, 
    lambda = 0.75
  )
  difference = median(pois) - mean(pois)
  return(difference)
}

# exponential distribution
exp_func <- function(n){
  exp = rexp(
    n, 
    rate = 0.5
  )
  difference = median(exp) - mean(exp)
  return(difference)
}

# Task 2: Build one data frame that contains all my simulations

# n = 10

# discrete
sim_result_discrete_10 <- replicate( # replicates the results of the discrete function 10000 times
  n = 10000,
  expr = discrete_func(10)
)

# normal
sim_result_norm_10 <- replicate(
  n = 10000,
  expr = norm_func(10)
)

# poisson
sim_result_pois_10 <- replicate(
  n = 10000,
  expr = pois_func(10)
)

# exponential
sim_result_exp_10 <- replicate(
  n = 10000,
  expr = exp_func(10)
)

# n = 50

# discrete
sim_result_discrete_50 <- replicate(
  n = 10000,
  expr = discrete_func(50)
)

# normal
sim_result_norm_50 <- replicate(
  n = 10000,
  expr = norm_func(50)
)

# poisson
sim_result_pois_50 <- replicate(
  n = 10000,
  expr = pois_func(50)
)

# exponential
sim_result_exp_50 <- replicate(
  n = 10000,
  expr = exp_func(50)
)

# n = 100

# discrete
sim_result_discrete_100 <- replicate(
  n = 10000,
  expr = discrete_func(100)
)

# normal
sim_result_norm_100 <- replicate(
  n = 10000,
  expr = norm_func(100)
)

# poisson
sim_result_pois_100 <- replicate(
  n = 10000,
  expr = pois_func(100)
)

# exponential
sim_result_exp_100 <- replicate(
  n = 10000,
  expr = exp_func(100)
)

# turn all replications into data frames
discrete_10 <- data_frame(sim_result_discrete_10)
discrete_50 <- data_frame(sim_result_discrete_50)
discrete_100 <- data_frame(sim_result_discrete_100)
normal_10 <- data_frame(sim_result_norm_10)
normal_50 <- data_frame(sim_result_norm_50)
normal_100 <- data_frame(sim_result_norm_100)
poisson_10 <- data_frame(sim_result_pois_10)
poisson_50 <- data_frame(sim_result_pois_50)
poisson_100 <- data_frame(sim_result_pois_100)
exp_10 <- data_frame(sim_result_exp_10)
exp_50 <- data_frame(sim_result_exp_50)
exp_100 <- data_frame(sim_result_exp_100)

# combine all the data frames into once
combined_mc_frame <- bind_cols(
  discrete_10,
  discrete_50,
  discrete_100,
  normal_10,
  normal_50,
  normal_100,
  poisson_10,
  poisson_50,
  poisson_100,
  exp_10,
  exp_50,
  exp_100
)

combined_mc_frame_clean <- combined_mc_frame %>%
  rename( # rename columns
    Discrete_10 = sim_result_discrete_10,
    Discrete_50 = sim_result_discrete_50,
    Discrete_100 = sim_result_discrete_100,
    Normal_10 = sim_result_norm_10,
    Normal_50 = sim_result_norm_50,
    Normal_100 = sim_result_norm_100,
    Poisson_10 = sim_result_pois_10,
    Poisson_50 = sim_result_pois_50,
    Poisson_100 = sim_result_pois_100,
    Exponential_10 = sim_result_exp_10,
    Exponential_50 = sim_result_exp_50,
    Exponential_100 = sim_result_exp_100
  ) %>%
  pivot_longer( # turn columns to rows
    cols = Discrete_10:Exponential_100,
    names_to = "Distribution",
    values_to = "Median - Mean"
  ) %>%
  separate_wider_delim( # separate distribution type and sample size
    cols = Distribution,
    delim = "_",
    names = c("Distribution", "Sample Size")
  )
View(combined_mc_frame_clean)

# Task 3: Create a plot

ggplot(
  data = combined_mc_frame_clean,
  mapping = aes(
    x = `Sample Size`,
    y = `Median - Mean`,
    color = Distribution
  )
) +
  geom_boxplot()