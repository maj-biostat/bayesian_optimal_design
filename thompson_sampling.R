

# uniform prior
a <- b <- 1

# 5 arms
k <- 5
d <- data.frame(p = c(0.3, 0.3, 0.3, 0.3, 0.4),
                n0 = rep(0, k),
                n1 = rep(0, k))

for(i in 1:10000){
  # sample from the posterior
  d$wa <- rbeta(k, a + d$n1, b + d$n0)
  
  # identify the greatest draw
  a_idx <- which.max(d$wa)
  
  # generate data using the arm with the 
  # highest draw
  y <- rbinom(1, 1, d$p[a_idx])
  
  # update the prior
  if(y == 0) d$n0[a_idx] <- d$n0[a_idx] + 1
  else d$n1[a_idx] <- d$n1[a_idx] + 1
}
d







