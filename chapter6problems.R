# Chapter 6

library(dagitty)
dag_6.1 <- dagitty( "dag {
                    U [unobserved]
                    X -> Y
                    X <- U <- A -> C -> Y
                    U -> B <- C
}")
adjustmentSets(dag_6.1, exposure = 'X', outcome = 'Y')

# 6M1
dagm1 <- dagitty( "dag {
                    U [unobserved]
                    X -> Y
                    X <- U <- A -> C -> Y
                    U -> B <- C
                    C <- V -> Y
}")
plot(dagm1)

# Paths from X to Y
# Before:
# X <- U <- A -> C -> Y
# X <- U -> B <- C -> Y
# After:
# X <- U <- A -> C -> Y
# X <- U -> B <- C -> Y
# X <- U <- A -> C <- V -> Y
# X <- U -> B <- C <- V -> Y
# 4 paths now
# Which are closed?: 
# 2nd and 4th have colliders (B), so they are closed.
adjustmentSets(dagm1, exposure = 'X', outcome = 'Y')
# Which variables should you condition on now?:
# A or (C and V)

# 6H1
data(WaffleDivorce)
