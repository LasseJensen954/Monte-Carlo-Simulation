### EXERCISE 1.8
sim.Pi <- function(R){
  set.seed(0)
  U <- runif(R)
  Z <- 4 * sqrt(1 - U^2)
  Z.exp <- mean(Z)
}

a.1 <- sim.Pi(1e6)

a.2 <- sim.Pi(1e5)

a.3 <- sim.Pi(1e5)
