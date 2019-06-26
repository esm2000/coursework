# Section 2.9
# ----------------------------------------------------------------

# 2.2:
# a)

control_dr <- 30/34    # 0.88
treatment_dr <- 45/69  # 0.65

# b) 
# i) N U L L : Heart transplants do not improve the survival rates
#              of  patients
#    A L T E R N A T E : Heart transplants increase the survival
#                        rates of patients

# ii) We write alive on 28 cards representing patients who were
#     alive at the end of the study, and dead on 75 cards
#     representing patients who were not. Then, we shuffle
#     these cards and split them into two groups: one group of 
#     size 69 representing treatment and another group of size
#     34 representing control. We calculate the difference between
#     the proportion of dead cards in the treatment and control groups
#     (treatment - control) and record this value. We repeat this
#     many times to build a distribution centered at 0. Lastly,
#     we calculate the fraction of simulations where the simulated
#     differences in proportions are two standard deviations away
#     from the expectation of the distribution. If the fraction
#     is low, we conclude that it is unlikely to have observed such
#     an outcome by chance and that the null hypothesis should be
#     rejected in favor of the alternative.

# iii)  the simulations results suggest that the transplant program
#       is actually slightly harmful.


# 2.5: 
# a)
cardio_prop <- 7979 / 227571 # 0.035

#b) 
0.035 * 67593 # 2365.755

#ci) The claims being tested are
# NULL - Rosiglitazone has no effect on cardiovascular problems.
# ALTERNATE - Rosiglitazone reduces cardiovascular problems.

# cii) fewer patients with cardiovascular problems in the 
#      Rosiglitazone group.

# ciii) 2400/67593 = 0.0355 
# the simulation results suggest that there is no relationship 
# between taking rosiglitazone and having crdiovascular problems
# in diabetic patients

# 2.21
# a) 
june_prob <- pnorm(83, mean = 77, sd = 5) 
june_prob <- 1 - june_prob # 0.1150697
# b) 
june_cold <- qnorm(0.1, mean = 77, sd = 5) # 70.59 degrees

# 2.23
# a)
c_mean <- (77 - 32) * (5/9) # 25 degrees Celcius
c_sd <- 5 * (5/9)    # 2.77 degrees Celcius
# Normal(25,2.77^2)

# b)
1- pnorm(28,mean = 25,sd = 2.77)
# 0.139397
library(tidyverse)
hist(rnorm(30,mean = 25, sd = 2.77) )
# c) I got similar a in part (b) of this question and part (a)
# of the Exercise 2.21


