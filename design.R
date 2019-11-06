


run_design <- function(t_par){
  
  # l <- list()
  
  
  if(t_par$run_parallel){
    
    cl <- makeCluster(min(parallel::detectCores() - 2, 10), outfile="")
    clusterExport(cl, ls(), envir=environment())
    
    # This traverses the model parameter space, i.e. the different
    # true values that we use to generate data.
    l <- parLapply(cl, 1:nrow(t_par$prop_mu), fun = function(idx_d, t_par){
      # source needed so that each thread gets functions
      source("trial.R")
      source("compare_proportions.R")
      a <- array(0, dim = c(t_par$nsim, length(t_par$res_names)))
      colnames(a) <- t_par$res_names
      
      # Simulate 'nsim' individual trials
      for(idx_e in 1:t_par$nsim){
        # set.seed(1); idx_e = 1
        # idx_e = idx_e + 1
        a[idx_e, ] <- run_trial(t_par, idx_d, idx_e)
      }
      
      a
    }, t_par)
    
    stopCluster(cl)
    
  } else {
    
    l <- lapply(1:nrow(t_par$prop_mu), FUN = function(idx_d, t_par){
      
      a <- array(0, dim = c(t_par$nsim, length(t_par$res_names)))
      colnames(a) <- t_par$res_names
      
      # Simulate 'nsim' individual trials
      for(idx_e in 1:t_par$nsim){
        # set.seed(1); idx_e = 1
        # idx_e = idx_e + 1
        a[idx_e, ] <- run_trial(t_par, idx_d, idx_e)
      }
      
      a
    }, t_par)
    
  }

  # Determine the frequentist operating characteristics
  oc <- operating_char(t_par, l)
  
  # Compute objective function for this design
  obj_f <- obj_func(t_par, oc, wgt = t_par$wgt)

  lres <- list(trial_results = l,
               oper_char = oc,
               obj_f = obj_f)
  
  lres
}

