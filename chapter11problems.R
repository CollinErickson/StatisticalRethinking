# Chapter 11
library(rethinking)
library(dplyr)
library(magrittr)
library(ggplot2)

# Problems are for Chapter 11 (version 2),
# but are listed as 10xx.

# 10E1
curve(logit)
logit(.35)
# -.619

# 10E2
curve(inv_logit, from=-10,to=10)
# 3.2 = log(p/(1-p))
# p is inv_logit(3.2)
inv_logit(3.2)
# .961

# 10E3
# log odds are multiplied by exp(1.7)
exp(1.7)

# 10E4
# An offset is used to convert count to rate.
# In the monk example, the manuscripts are counted over different time periods,
# by day and by week. To put them on a comparable scale, we need an set to
# correct for this time difference.


# 10M1
# The aggregated version has the n choose k multiplier.

# 10M2
# changing the predictor by one unit increases the log odds by exp(1.7)

# 10M3
# Binomial needs a probability, which must be within 0 to 1.
# Using the logit link ensures the values are in 0 to 1.

# 10M4
# For Poisson, lambda must be positive. The log link
# can map the linear model output  onto the positive numbers.

# 10M5
# The rate must be between 0 and 1.
# A process known to have mean below 1.

# 10M6
# Binomial: Each trial has two outputs, with fixed mean.
# Poisson: Fixed mean, independent events in a fixed time.

