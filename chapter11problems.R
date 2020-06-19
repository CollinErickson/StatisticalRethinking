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
inv_logit(3.2)
# .961

# 10E3

# 10E4
# An offset is used to convert count to rate.
# In the monk example, the manuscripts are counted over different time periods,
# by day and by week. To put them on a comparable scale, we need an set to
# correct for this time difference.
