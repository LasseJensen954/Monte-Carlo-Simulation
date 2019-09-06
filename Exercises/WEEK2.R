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
lambda <- 1/2

invNorm <- function(x) {
  1 / sqrt(2* pi * x^3) * exp((-(x - 1)^2) / (2 * x))
}

exp.d <- function(x, l) {
  l * exp(-l * x)
}

max.ar <- function(x){
  invNorm(x)/exp.d(x, lambda)
}

optimize(max.ar, interval=c(0,1), maximum=TRUE)

C <- invNorm(1/3)/exp.d(1/3, lambda)

x <- seq(0, 10, 1/1e3)

plot(x, invNorm(x), col = "red", type = "l", xlim = c(0,10), ylim = c(0,1.3))
lines(x, C*exp.d(x, lambda), col = "blue")

# A-R Algorithm
R <- 1e6

AR <- function(C) {
  repeat{
    y <- rexp(1, rate = lambda)
    u <- runif(1)
    
    if(u < invNorm(y)/(C*exp.d(y, lambda))) {
      return(y)
    }
  }
}

data <- replicate(R, AR(C))

hist(data, prob = TRUE, breaks = 150, xlim = c(0, 10))
lines(x, invNorm(x), col = "red")


### EXERCISE 12
set.seed(0)

n <- 1e5
p <- 0.01

sim <- rgeom(n, p)
mean(sim)+1

U <- runif(n)

sim2 <- function(U, p) {
  a <- ceiling(log(U)/log(1-p))
}

X <- sim2(U, p)
mean(X)

microbenchmark::microbenchmark(sim2(U, p), rgeom(n, p))

### EXERCISE 15
library(tidyverse)

sigma <- matrix(c(25, 15, 15, 18), 2, 2)

data <- MASS::mvrnorm(1e6, mu = c(0, 0), Sigma = sigma)

data <- tibble::tibble(x=data[,1], y=data[,2])

smoothScatter(data)

col <- densCols(data)

plot(data, col = col)

ggplot(data)+geom_hex(aes(x=x,y=y))

### Blackboard Exercise
library(tidyverse)

n <- 1e6
set.seed(0)

data <- tibble::tibble(X=rexp(n), Y=rexp(n))

data <- data %>% filter(Y< 1 & X+Y>1)

hist(data$Y, prob = TRUE, breaks = 110)

plot(density(data$Y))

ggplot(data)+geom_hex(aes(x=X, y=Y))
