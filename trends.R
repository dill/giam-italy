
library(mvtnorm)

sig.lev <- 0.05
n.rep   <- 1000
s.meth  <- "svd"

bs  <- rmvnorm(1, mean = b$coeff, sigma=b$Vp, method=s.meth)

# 

median(RES)
quantile(RES,c(sig.lev/2,1-sig.lev/2))













