# True parameters setup
beta_pars <- function(mu, v0){
  a = (((1-mu)/v0^2) - (1/mu))*mu^2
  b = a*((1/mu)-1)
  list(a=a, b=b)
}

beta_moment <- function(a, b){
  mu <- a/(a+b)
  sig2 <- a*b/(((a+b)^2) * (a+b+1))
  list(mu = mu, sig2 = sig2)
}

# Assumes we are evaluating p1 > p2
delta_moment <- function(a1, b1, a2, b2){
  
  m1 <- beta_moment(a1, b1)
  m2 <- beta_moment(a2, b2)
  
  del_mu <- m1$mu - m2$mu
  del_sig2 <- (m1$mu*(1-m1$mu)/(a1+b1+1)) + (m2$mu*(1-m2$mu)/(a2+b2+1))
  
  list(mu = del_mu, 
       sig2 = del_sig2)
}

prob_delta_gt_0 <- function(delta_mu, delta_sig2){
  z <- (0 - delta_mu) / sqrt(delta_sig2)
  
  # If prob_theta = Pr(pi1 > pi2 | evt1, evt2)
  prob_theta <- 1 - pnorm(z)
  prob_theta
}

frequentist <- function(){
  
  # Frequentist perspective on power
  p1 <- 0.4
  p2 <- 0.5
  # significance level
  alpha <- 0.1
  
  power.prop.test(n = NULL, p1, p2, sig.level = alpha,
                  power = 0.8,
                  alternative = c("one.sided"),
                  strict = FALSE)
  
  n <- seq(0, 50, by = 5)
  pwr <- numeric(length(n))
  i=1
  for(i in 1:length(n)){
    test_pwr <- power.prop.test(n = n[i], p1, p2, sig.level = alpha,
                                alternative = c("one.sided"),
                                strict = FALSE)
    pwr[i] <- test_pwr$power
  }
  pwr
  plot(n, pwr, type = "l", ylim = c(0, 1))
  
  # prop.test(matrix(c(50, 30, 100, 100), nrow = 2, byrow = T), alternative = c("greater"))
}

# Operating characteristics for equivalent Bayesian trial
bayes_trial <- function(){
  
  # Frequentist perspective on power
  p1 <- 0.4
  p2 <- 0.5
  # significance level
  alpha <- 0.1
  
  power.prop.test(n = NULL, p1, p2, sig.level = alpha,
                  power = 0.8,
                  alternative = c("one.sided"),
                  strict = FALSE)
  
  power.prop.test(n = 35, p1, p2, sig.level = alpha,
                  power = NULL,
                  alternative = c("one.sided"),
                  strict = FALSE)

  # Equivalent Bayesian analysis to static two arm frequentist trial
  # 223 individuals gives 80% power when p0=0.4, p1=0.5, alpha = 0.1.
  # If you set the second group to 0.4 instead of 0.5 then the false
  # positive rate is 0.1.
  nsim <- 1000
  ntest <- 223 # per arm
  zwin <- 0.9
  gt_0_win <- numeric(nsim)
  for(i in 1:nsim){
    y1 <- rbinom(1, ntest, p2)
    y2 <- rbinom(1, ntest, p1)
    
    d0 <- delta_moment(a1 = y1 + 1, b1 = ntest - y1 + 1,
                       a2 = y2 + 1, b2 = ntest - y2 + 1)
    
    gt_0_win[i] <- prob_delta_gt_0(d0$mu, d0$sig2) > zwin
  }
  mean(gt_0_win)
  
  
  nsim <- 1000
  ntest <- 50 # per arm
  zwin <- 0.9
  gt_0_win <- numeric(nsim)
  for(i in 1:nsim){
    y1 <- rbinom(1, ntest, p2)
    y2 <- rbinom(1, ntest, p1)
    
    d0 <- delta_moment(a1 = y1 + 1, b1 = ntest - y1 + 1,
                       a2 = y2 + 1, b2 = ntest - y2 + 1)
    
    gt_0_win[i] <- prob_delta_gt_0(d0$mu, d0$sig2) > zwin
  }
  mean(gt_0_win)
}


bayes_approx <- function(){
  # The primary analysis is to conclude statistical success if the 
  # posterior probability of p2 > p1 is greater than 95%.
  # Assuming a uniform prior and observed 50 and 30 events
  # the difference is 
  x <- seq(0, 1, len = 1000) 
  trials <- 1000
  evt1 <- 500
  evt2 <- 480
  post1 <- dbeta(x, evt1 + 1, trials - evt1 + 1)
  post2 <- dbeta(x, evt2 + 1, trials - evt2 + 1)
  # plot(x, post1, type = "l")
  # lines(x, post2)
  
  # 
  d0 <- delta_moment(a1 = evt1 + 1, b1 = trials - evt1 + 1,
                     a2 = evt2 + 1, b2 = trials - evt2 + 1)
  x <- seq(-1, 1, len = 1000) 
  post3 <- dnorm(x, d0$mu, sqrt(d0$sig2))
  plot(x, post3, type = "l")
  abline(v = 0)
  
  prob_gt(d0$mu, d0$sig2)
 
}


bayes_mc <- function(){
  # You can also approach this with a Monte Carlo method 
  be1 <- rbeta(10000, evt1 + 1, trials - evt1 + 1)
  be2 <- rbeta(10000, evt2 + 1, trials - evt2 + 1)
  mean(be1 > be2)
}



