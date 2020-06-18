# Chapter 11
library(rethinking)
library(dplyr)
library(magrittr)
library(ggplot2)

# 10E1
curve(logit)
logit(.35)
# -.619

# 10E2
curve(inv_logit, from=-10,to=10)
inv_logit(3.2)
# .961

# 10E3

