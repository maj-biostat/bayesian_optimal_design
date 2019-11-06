library(ggplot2)

list.files()

decision_space <- readRDS("fut\\decision_space.RDS")
lresults <- readRDS("fut\\results.RDS")
t_par <- readRDS("fut\\t_par.RDS")
max_des <- readRDS("fut\\max_des.RDS")

head(decision_space)

wgt = c(3, 1, 1)

decision_space$w1 <- wgt[1]*decision_space$r1
decision_space$w2 <- wgt[2]*decision_space$r2
decision_space$w3 <- wgt[3]*decision_space$r3

decision_space$best <- ""
decision_space$best[max_des$max_des_idx_des] <- "*"





summary(decision_space)

power.prop.test(n = 50, 0.6, 0.3, sig.level = 0.1,
                power = NULL,
                alternative = c("one.sided"),
                strict = FALSE)

length(lresults[[max_des$max_des_idx_des]]$trial_results)
dcmp <- as.data.frame(lresults[[max_des$max_des_idx_des]]$trial_results[[3]])
mean(dcmp$win)

# label facets
anat <- sort(unique(decision_space$nanalyses))
decision_space$num_analyses <- factor(paste0(decision_space$nanalyses , 
                                             " analyses"),
                                      levels = paste0(anat, " analyses"))

# fill as factor
# length(unique(decision_space$obj_f))
# colors <- colorRampPalette(c("blue", "green", 
#                              "yellow", "red"))(length(unique(decision_space$obj_f)))

unique(decision_space$zwin)
unique(decision_space$wfut)
ggplot(decision_space, aes(zwin, wfut, label = best)) +
  geom_tile(aes(fill = obj_f)) +
  geom_text(size = 8, colour = "red") +
  scale_y_continuous("Futility threshold") +
  scale_x_continuous("Probability for Success") + 
  scale_fill_gradient(low = "#000000", high = "#FDFDFD",
                      aesthetics = "fill") +
  # scale_fill_manual(values=colors) +
  # theme(legend.position="none") +
  facet_grid(num_analyses ~ paste0("Start @ ", nmin)) +
  ggtitle("Objective Function (higher is better; * is best)")

ggplot(decision_space, aes(zwin, wfut, label = best)) +
  geom_tile(aes(fill = false_pos)) +
  geom_text(size = 8, colour = "red") +
  scale_y_continuous("Futility threshold") +
  scale_x_continuous("Probability for Success") + 
  scale_fill_gradient(low = "#000000", high = "#FDFDFD",
                      aesthetics = "fill") +
  # scale_fill_manual(values=colors) +
  # theme(legend.position="none") +
  facet_grid(num_analyses ~ paste0("Start @ ", nmin)) +
  ggtitle("False +ve (closer to alpha is better)")

ggplot(decision_space, aes(zwin, wfut, label = best)) +
  geom_tile(aes(fill = ss)) +
  geom_text(size = 8, colour = "red") +
  scale_y_continuous("Futility threshold") +
  scale_x_continuous("Probability for Success") + 
  scale_fill_gradient(low = "#000000", high = "#FDFDFD",
                      aesthetics = "fill") +
  # scale_fill_manual(values=colors) +
  # theme(legend.position="none") +
  facet_grid(num_analyses ~ paste0("Start @ ", nmin)) +
  ggtitle("Sample Size (lower is better)")

ggplot(decision_space, aes(zwin, wfut, label = best)) +
  geom_tile(aes(fill = pwr)) +
  geom_text(size = 8, colour = "red") +
  scale_y_continuous("Futility threshold") +
  scale_x_continuous("Probability for Success") + 
  scale_fill_gradient(low = "#000000", high = "#FDFDFD",
                      aesthetics = "fill") +
  # scale_fill_manual(values=colors) +
  # theme(legend.position="none") +
  facet_grid(num_analyses ~ paste0("Start @ ", nmin)) +
  ggtitle("Power (higher is better)")

