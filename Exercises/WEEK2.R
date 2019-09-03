### EXERCISE 1.8
sim.Pi <- function(R){
  set.seed(0)
  U <- runif(R)
  Z <- 4 * sqrt(1 - U^2)
  Z.exp <- mean(Z)
}

a.1 <- sim.Pi(1e5)

a.2 <- sim.Pi(1e5)

### EXERCISE 1.9

gumbel.p <- function(x){
  dist <- exp(-exp(-x))
}

gumbel.q <- function(p){
  quan <- -log(-log(p))
}

# Simulate 1e4 Gumbel points.
x <- runif(1e4)
gumbel.sim <- gumbel.q(x)

# Find 1e4 true Gumbel points.
p <- seq(0, 1, 1/1e4)
gumbel.true <- gumbel.q(p)

# Plot histogram.
hist(gumbel.sim, prob = TRUE, ylim = c(0, 0.4))

lines(density(gumbel.sim), col = "red")
lines(density(gumbel.true), col = "blue")

qqplot(gumbel.true, gumbel.sim)
abline(0, 1, col = "red")

### EXERCISE 1.10
sim <- rnorm(1e5)
sim <- sim[sim > 1.645]

prop <- 1 - length(sim)/1e5

sim.mean <- mean(sim)
sim.mean

# MC Integration
alpha <- 0.05

X <- runif(1e5, 0, alpha)
Z <- -qnorm(X)
Z.mean <- mean(Z)
Z.mean

### EXERCISE 1.11



