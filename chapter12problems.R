# Chapter 12
library(rethinking)
library(dplyr)
library(magrittr)
library(ggplot2)

# I have unfinished vol 2, so problems are number as they were in vol 1

# 11E1
# Ordered has a strict order...
# The distance between each category is unknown,
# but there is information gained from each category's
# relation to other categories.
# Unordered: Country
# Ordered: On a scale of 1 to 10, how do you feel about ...?

# 11E2
# Cumulative logit function.
# It's a logit on the cumulative probability.
# Since the categories are ordered, they are spaced out between 0 and 1
# on the cumulative scale."
# To find the effect for each category, you add the "probabilities"
# of all lower categories.

# 11E3
# It's a model misspecification.
# For Poisson, it'll underestimate lambda.

