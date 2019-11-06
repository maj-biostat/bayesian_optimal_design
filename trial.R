

# testing p1 > p2 
run_trial <- function(t_par, idx_d = 1, idx_sim = 0){
  
  stopifnot(idx_d <= nrow(t_par$prop_mu))
 
  res <- numeric(length(t_par$res_names))
  names(res) <- t_par$res_names
  
  res["simid"] <- idx_sim
  
  # Duplication but convenient
  res["p1"] <- t_par$prop_mu$p1[idx_d]
  res["p2"] <- t_par$prop_mu$p2[idx_d]
  res["zwin"] <- t_par$zwin
  res["wfut"] <- t_par$wfut
  
  y1 <- 0
  y2 <- 0
 
  for(idx_k in 1:length(t_par$n)){
    # idx_k = 1
    # idx_k = idx_k + 1
    if(idx_k == 1){
      y1 <- rbinom(1, t_par$n[idx_k], t_par$prop_mu$p1[idx_d])
      y2 <- rbinom(1, t_par$n[idx_k], t_par$prop_mu$p2[idx_d])
    } else {
      y1 <- y1 + rbinom(1, t_par$n[idx_k] - t_par$n[idx_k-1], t_par$prop_mu$p1[idx_d])
      y2 <- y2 + rbinom(1, t_par$n[idx_k] - t_par$n[idx_k-1], t_par$prop_mu$p2[idx_d])
    }
    
    # predictive sim loop
    pred_win <- numeric(t_par$nsimpred) # predictive wins
    
    if(t_par$enable_fut){
      
      for(idx_j in 1:t_par$nsimpred){
        # idx_j = 1
        
        # Posterior
        # theta|Y ~ Beta(y + a, n - y + b)
        # Posterior Predictive - simulate the next m obs with
        # Yhat|Y, theta ~ BetaBinom(y + a, n - y + b)
        # where a = 1, b = 1 implies uniform prior
        
        # The predictive distribution for the NEXT "npergrp - n[idx_k]" subjects
        # Check Christensen for this.
        y1hat <- y1 + extraDistr::rbbinom(1, t_par$npergrp - t_par$n[idx_k],
                                          y1 + 1, t_par$n[idx_k] - y1 + 1)
        y2hat <- y2 + extraDistr::rbbinom(1, t_par$npergrp - t_par$n[idx_k],
                                          y2 + 1, t_par$n[idx_k] - y2 + 1)
        
        # Work out the moments of the difference between the frequency of events
        # in each arm at the max sample size. Incorporate a uniform prior.
        # We are effectively saying "if we are at the maximum sample size and
        # these are the observations we make, then are the groups different based
        # on their posterior distributions?"
        d0 <- delta_moment(a1 = y1hat + 1, b1 = t_par$npergrp - y1hat + 1,
                           a2 = y2hat + 1, b2 = t_par$npergrp - y2hat + 1)
        
        # compare posterior assess if p1 > p2
        pred_win[idx_j] <- as.numeric(prob_delta_gt_0(d0$mu, d0$sig2) > t_par$zwin)
      }
    }
 
    # arm means
    pi1_mu <- beta_moment(a = y1 + 1, b = t_par$n[idx_k] - y1 + 1)$mu
    pi2_mu <- beta_moment(a = y2 + 1, b = t_par$n[idx_k] - y2 + 1)$mu
    
    # difference at this analysis time
    d0 <- delta_moment(a1 = y1 + 1, b1 = t_par$n[idx_k] - y1 + 1,
                       a2 = y2 + 1, b2 = t_par$n[idx_k] - y2 + 1)
    p_delta_gt_0 <- prob_delta_gt_0(d0$mu, d0$sig2)

    if(t_par$enable_fut & mean(pred_win) < t_par$wfut){
      res["ss_tot"] <- 2*t_par$n[idx_k]
      res["fut"] <- 1
      res["win"] <- 0
      res["pi1_mu"] <- pi1_mu
      res["pi2_mu"] <- pi2_mu
      res["diff_mu"] <- d0$mu
      res["prob_win"] <- mean(pred_win)
      res["prob_delta_gt_0"] <- p_delta_gt_0
      break
    }
    
    if(p_delta_gt_0 > t_par$zwin){
      res["ss_tot"] <- 2*t_par$n[idx_k]
      res["fut"] <- 0
      res["win"] <- 1
      res["pi1_mu"] <- pi1_mu
      res["pi2_mu"] <- pi2_mu
      res["diff_mu"] <- d0$mu
      res["prob_win"] <- mean(pred_win)
      res["prob_delta_gt_0"] <- p_delta_gt_0
      break
    }
    
    if(t_par$n[idx_k] == max(t_par$n)){
      res["ss_tot"] <- 2*max(t_par$n)
      res["fut"] <- 0
      res["win"] <- 0
      res["pi1_mu"] <- pi1_mu
      res["pi2_mu"] <- pi2_mu
      res["diff_mu"] <- d0$mu
      res["prob_win"] <- mean(pred_win)
      res["prob_delta_gt_0"] <- p_delta_gt_0
    }
    
  }
  
  res
  
}

# Assumes data source list exists
operating_char <- function(t_par, l){
  
  stopifnot(length(l) > 0)
  
  false_pos_mu <- 0
  ss_mu <- 0
  pwr_mu <- 0
  
  for(idx_d in 1:nrow(t_par$prop_mu)){
    # idx_d = 1; # idx_d = idx_d + 1
    p0 <- t_par$prop_mu$p1[idx_d]
    p1 <- t_par$prop_mu$p2[idx_d]

    d <- as.data.frame(l[[idx_d]])
    
    if(p0 == p1){
      # incrementally update means
      # false_pos takes the place of power if p0 == p1
      false_pos_mu <- false_pos_mu + ((mean(d$win) - false_pos_mu)/idx_d)
    } else {
      # the context now implies power
      pwr_mu <- pwr_mu + ((mean(d$win) - pwr_mu)/idx_d)
    }
    
    ss_mu <- ss_mu + ((mean(d$ss_tot) - ss_mu)/idx_d)
  }
  
  list(false_pos_mu = false_pos_mu,
       ss_mu = ss_mu,
       pwr_mu = pwr_mu)
}

obj_func <- function(t_par, oc, wgt = c(1, 1, 1)){
  
  # scale everything to 0 to 1 then weight by priority
  
  # distance from target alpha
  target_alpha_delta <- abs(t_par$alpha_target - oc$false_pos_mu)
  
  r1 <- 1 - target_alpha_delta
  r2 <- 1 - (oc$ss_mu / t_par$nmax)
  r3 <- oc$pwr_mu
  
  message("Raw Contributions      : ", paste(round(c(r1, r2, r3), 2), collapse = ", "))
  
  w1 <- wgt[1]*r1
  w2 <- wgt[2]*r2
  w3 <- wgt[3]*r3

  message("Obj func Contributions : ", paste(round(c(w1, w2, w3), 2), collapse = ", "))
  
  f <- w1 +    # less deviation from target better
    w2 +       # smaller sample size better
    w3         # higher power better
  
  list(f = f,
       raw = c(r1, r2, r3))
       
}

setup <- function(run_parallel = 0,  
                  enable_fut = 0, 
                  alpha_target = 0.1,
                  nsim = 1000, 
                  nsimpred = 1000,
                  nmin = 20,
                  nanalyses = 5,
                  zwin = 0.95,
                  wfut = 0.1,
                  wgt = c(1, 1, 1)){
  
  
  t_par <- list()
  
  t_par$run_parallel = run_parallel
  t_par$wgt = wgt
  
  t_par$prop_mu <- expand.grid(p1 = seq(0.3, 0.6, len = 3),
                               p2 = seq(0.3, 0.6, len = 3))
  
  # Total max sample size assumed to be split evenly alloc 
  # across groups.
  t_par$nmax <- 100
  t_par$npergrp <- t_par$nmax / 2
  t_par$nanalyses <- nanalyses
  t_par$nmin <- nmin

  # How many ways to allocate nmax individs into nanalyses groups?
  # Quite a lot
  t_par$how_many <- choose(t_par$npergrp - 1, t_par$nanalyses - 1)
  
  # assess posterior at a prob of:
  t_par$zwin <- zwin
  # assess predictive fut at a prob of:
  t_par$wfut <- wfut
  # success is based on post at interim - no missing dat
  # wexpsuc <- 0.9
  
  # target false positive rate
  t_par$alpha_target <- 0.1
  
  # Q. How to split the sample size into interims?
  
  # The interim and final analyses at n_k ***per group***
  t_par$n <- round(seq(t_par$nmin/2, t_par$npergrp, length.out = t_par$nanalyses), 0)
  # The following is not reqd because I set the max in seq to be npergrp.
  # t_par$n[length(t_par$n)] <- ifelse(t_par$n[length(t_par$n)] != t_par$npergrp, 
  #                                    t_par$npergrp, t_par$n[length(t_par$n)])
  
  stopifnot(max(t_par$n) == t_par$npergrp)
  
  
  t_par$nsim <- nsim
  t_par$nsimpred <- nsimpred
  t_par$res_names <- c("simid", 
                       "p1", "p2", "zwin", "wfut",
                       "ss_tot", 
                       "fut", "win", 
                       "pi1_mu", "pi2_mu", 
                       "diff_mu",
                       "prob_win", 
                       "prob_delta_gt_0")
  
  t_par$opchar_names <- c("false_pos_mu", "ss_mu", "pwr_mu")
  
  t_par$enable_fut <- enable_fut
  
  t_par
}