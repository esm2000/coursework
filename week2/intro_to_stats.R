# Exercise 7.1
# ---------------------------------------------------------------

pop2 <- read_csv('pop2.csv')


str(pop2)
summary(pop2)

# Compute the population average of bmi
mean(pop2$bmi) # 24.98446

# Compute the population standard deviation of bmi
sd(pop2$bmi) # 4.188511

# Compute the expectation of the sample distribution of the sample
# average of the variable

mean_expectation <- rep(0,150)
for(i in 1:150)
{
  sample_bmi <- sample(pop2$bmi,150)
  mean_expectation[i] <- mean(sample_bmi)
}
hist(mean_expectation)
mean(mean_expectation) # 24.96333

# Compute the standard deviation of the sample distribution 
# of the sample average of the variable

sd(mean_expectation) # 0.3091336

# Identify, using simulations,the central region that contains
# 80% of the sample distribution of the average

quantile(mean_expectation,c(0.1,0.9)) # 24.56249-25.36853

# Identify, using central limit theorem, and approximation 
# of the central region that contains 80% of the sample distribution
# of the average

qnorm(c(0.1,0.9), mean = mean(mean_expectation),
      sd = sd(mean_expectation))
# 24.56716-25.35950

# Exercise 9.1
# --------------------------------------------------------------

magnets <- read_csv('magnets.csv')
str(magnets)
summary(magnets)

# What is the sample average of the change in patient pain scores?
mean(magnets$change) # 3.5

# Is the variable 'active' a factor or a numeric variable?

str(magnets$active)
# the variable 'active' is a factor, since we are not interested
# in its test statistics

# Compute the average value of the variable "change" for the 
# patients that received and active magnet and average value
# for those that received an inactive placebo.

library(tidyverse)
magnets_active <- magnets %>%
  filter(active == '\"1\"')

magnets_control <- magnets %>%
  filter(active != '\"1\"')

mean(magnets_active$change) # 5.241379
mean(magnets_control$change) # 1.095238

# Compute the standard deviation of the variable "change" for the 
# patients that received and active magnet and average value
# for those that received an inactive placebo.

sd(magnets_active$change) # 3.236568
sd(magnets_control$change) # 1.578124

# Produce a boxplot

boxplot(change ~ active, magnets)

# Active patients have no outliers while control patients have
# 3 outliers

# Exercise 9.2
# --------------------------------------------------------------

# Assume that the expectation of the measurement is equal
# to 3.5, regardless of what the type of treatment that 
# the patient received. We take the
# standard deviation of the measurement for patients the 
# receives an active magnet to be equal to 3 and for those
# that received the inactive placebo we take it to be equal 
# to 1.5. Assume that the distribution of the measurements is
# Normal and there are 29 patients in the first group and 21 
# in the second. Find the interval that contains 95% of the
# sampling distribution of the statistic.

active_sim <- rnorm(29, 3.5, sd = 3)
passive_sim <- rnorm(21, 3.5, sd = 1.5)
total_sim <- append(active_sim, passive_sim)

low_95 <- mean(total_sim) - (1.96 * sd(total_sim))
high_95 <- mean(total_sim) / + (1.96 * sd(total_sim))
confidence_95 <- c(low_95,high_95)

confidence_95 # -1.5756877 - 0.7085026


