library(acebayes)
utilfisher <- function(d, B) {
  # prior N(0, 1)
  theta <- rnorm(B)
  
  ui <- matrix(rep(d[, 1] ^ 2, B), ncol = B) * exp(outer(d[, 1], theta))
  apply(ui, 2, sum)
}

set.seed(1)
n <- 12
start.d <- matrix(0, nrow = n, ncol = 1)
ex22a <- ace(utility = utilfisher, start.d = start.d)
str(ex22a)


n_ptcl <- 1
theta_1 <- runif(n_ptcl, 0.01884, 0.9884)
theta_2 <- runif(n_ptcl, 0.298, 8.298)
theta_3 <- 21.8

t <- seq(0, 24, length.out = 100)

mu <- theta_3*(exp(-theta_1*t) - exp(-theta_2*t))

plot(t, mu)





set.seed(1)
n <- 18
k <- 1
p <- 3
start.d <- randomLHS(n = n, k = k) * 24
colnames(start.d) <- c("t")

a1 <- c(0.01884, 0.298)
a2 <- c(0.09884, 8.298)
prior <- list(support = cbind(rbind(a1, a2), c(21.8, 21.8)))
colnames(prior[[1]]) <- c("theta1", "theta2", "theta3")


ex411 <- acenlm(formula = ~ theta3 * (exp(-theta1*t) - exp(-theta2*t)), 
                start.d = start.d, prior = prior, lower = 0,
                upper = 24)


length(unique(ex411$phase2.d))


limits <- function(d, i, j) {
  grid<-seq(from = 0, to = 24, length.out = 10000)
  for(s in as.vector(d)[-i]) {
    grid <- grid[(grid < (s - 0.25)) | (grid > (s + 0.25))]
  }
  grid
}

C <- 10
start.d <- list()
for(i in 1:C){
  start.d[[i]] <- randomLHS(n = n, k = k) * 24
  colnames(start.d[[i]]) <- c("t")
}
ex412b <- pacenlm(formula = ~ theta3 * (exp(-theta1 * t) - exp(-theta2 * t)), 
                  start.d = start.d, prior = prior, lower = 0,
                  upper = 24, limits = limits, N2 = 0)


plot(sort(ex412b$d))



# LOGISTIC REGRESSION

# ONES I prepared earlier
ex411 <- readRDS("ex411.RDS")
ex412 <- readRDS("ex412.RDS")
plot(ex411)
plot(ex412)

ex411$d
ex412$d


set.seed(1)
n <- 6
p <- 5
k <- 4
C <- 10
start.d <- list()
for(i in 1:C){
  start.d[[i]] <- randomLHS(n = n, k = k) * 2 - 1
  colnames(start.d[[i]]) <- c("x1", "x2", "x3", "x4")
  }
a1 <- c(-3, 4, 5, -6, -2.5)
a2 <- c(3, 10, 11, 0, 3.5)
prior <- list(support = rbind(a1, a2))

ex411 <- paceglm(formula = ~ x1 + x2 + x3 + x4, family = binomial,
                 start.d = start.d, prior = prior, criterion = "A")

prior <- function(B) {
  theta <- matrix(0, nrow = B, ncol = p)
  for(b in 1:B) {
    theta[b, ] <- runif(n = p, min = a1, max = a2)
  }
  theta
}

ex412 <- paceglm(formula = ~ x1 + x2 + x3 + x4, family = binomial,
                 start.d = start.d, prior = prior, criterion = "NSEL-Norm")

plot(ex412)


