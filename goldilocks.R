library(extraDistr)
library(parallel)
source("compare_proportions.R")
source("trial.R")
source("design.R")

# Just a reminder to mj. Ignore this first para.
# Consider some experiment where patients allocated to active and placebo 
# in pairs. The experiment tests whether the active patient responds 
# better than the placebo patient (with no draws).
# Now you can use this expt to compute the probability that the active is
# better than the placebo using a beta binomial.
# See Berry 96 sec 7.2 (Statistics: A Bayesian Perspective) and 
# Berry 2010 around pg 36.



# These are the things that we are allowing to change. We are trying
# to decide which combination of these represent a 'good' calibration
# for our design aims encoded in the objective function.
# decision_space <- expand.grid(nmin = c(20, 30, 40),
#                               nanalyses = c(2, 3, 4, 5, 7, 10),
#                               # probability required to declare win
#                               zwin = c(0.85, 0.875, 0.9, 0.925, 0.95, 0.975),
#                               # trial futile if predictive probs less than
#                               # these bounds
#                               wfut = c(0.05, 0.1, 0.15, 0.2))

decision_space <- expand.grid(nmin = c(20, 40),
                              nanalyses = c(2, 5, 10),
                              # probability required to declare win
                              zwin = c(0.85, 0.9, 0.95),
                              # trial futile if predictive probs less than
                              # these bounds
                              wfut = c(0.05, 0.1, 0.15, 0.2))

decision_space$false_pos <- NA
decision_space$ss <- NA
decision_space$pwr <- NA
decision_space$obj_f <- NA
decision_space$r1 <- NA
decision_space$r2 <- NA
decision_space$r3 <- NA

# two 3x4 matrices indexed by third dim.
# array(0, dim = c(3, 4, 2))



lresults <- list()
max_des <- list(obs_f = 0,
                idx_des = 0)


for(i in 1:nrow(decision_space)){
  # i = 1
  t_par <- setup(run_parallel = 0,
                 enable_fut = 0, 
                 alpha_target = 0.1,
                 nsim = 1000, 
                 nsimpred = 1000, 
                 nmin = decision_space[i, "nmin"],
                 nanalyses = decision_space[i, "nanalyses"],
                 zwin = decision_space[i, "zwin"],
                 wfut = decision_space[i, "wfut"],
                 wgt = c(3, 1, 1))
  
  if(i == 1){
    a_msg <- paste0("Size of decision space : ", nrow(decision_space) , "\n",
                    "Futility check enabled : ", t_par$enable_fut , "\n",
                    "Running in parallel    : ", t_par$run_parallel , "\n",
                    "Objective weights      : ", paste(t_par$wgt, collapse = ", "))
    message(a_msg)
  }
  

  lresults[[i]] <- run_design(t_par)
  
  # crappy practice but convenient
  decision_space$false_pos[i] <- lresults[[i]]$oper_char$false_pos_mu
  decision_space$ss[i] <- lresults[[i]]$oper_char$ss_mu
  decision_space$pwr[i] <- lresults[[i]]$oper_char$pwr_mu
  decision_space$obj_f[i] <- lresults[[i]]$obj_f$f
  decision_space$r1[i] <- lresults[[i]]$obj_f$raw[1]
  decision_space$r2[i] <- lresults[[i]]$obj_f$raw[2]
  decision_space$r3[i] <- lresults[[i]]$obj_f$raw[3]
  
  
  message("Obj func for design ", i, " : ", round(lresults[[i]]$obj_f$f, 3), 
          " false+ ", round(lresults[[i]]$oper_char$false_pos_mu, 6),
          " ss ", round(lresults[[i]]$oper_char$ss_mu, 2),
          " pwr ", round(lresults[[i]]$oper_char$pwr_mu, 2))
  
  if(max_des$obs_f < lresults[[i]]$obj_f$f){
    max_des$obs_f <- lresults[[i]]$obj_f$f
    max_des$idx_des <- i
  }

}

a_msg <- paste0("Size of decision space : ", nrow(decision_space) , "\n",
                "Futility check enabled : ", t_par$enable_fut , "\n",
                "Running in parallel    : ", t_par$run_parallel , "\n",
                "Objective weights      : ", paste(t_par$wgt, collapse = ", "))
message(a_msg)

res_msg <- paste0("Design maximising objective func", "\n\n",
                  " decision_space index : ", max_des$idx_des, "\n",
                  " objective function   : ", max_des$obs_f, "\n",
                  " nmin                 : ", decision_space[max_des$idx_des, "nmin"], "\n",
                  " nanalyses            : ", decision_space[max_des$idx_des, "nanalyses"], "\n",
                  " zwin                 : ", decision_space[max_des$idx_des, "zwin"], "\n",
                  " wfut                 : ", decision_space[max_des$idx_des, "wfut"], "\n\n",
                  "Operating characteristics", "\n",
                  " false +ve            : ", round(lresults[[max_des$idx_des]]$oper_char$false_pos_mu, 6), "\n",
                  " expected ss          : ", round(lresults[[max_des$idx_des]]$oper_char$ss_mu, 3), "\n",
                  " expected pwr         : ", round(lresults[[max_des$idx_des]]$oper_char$pwr_mu, 3), "\n\n",
                  "Model parameters", "\n",
                  " p0                   : ", paste(t_par$prop_mu$p1, collapse = ", "), "\n",
                  " p1                   : ", paste(t_par$prop_mu$p2, collapse = ", "))
message(res_msg)

saveRDS(decision_space, "decision_space.RDS")
saveRDS(lresults, "results.RDS")
saveRDS(list(max_des_idx_des = max_des$idx_des,
             max_des_obs_f = max_des$obs_f), "max_des.RDS")
saveRDS(t_par, "t_par.RDS")


