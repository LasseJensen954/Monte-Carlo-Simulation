#### ANDREAS KRACHT FRANDSEN.

### EXERCISE 1.1.

# Generate uniform RV.
a <- runif(1000)

# Make slow function.
slow <- function(a) {
  g <- c()
  for (i in seq_along(a)) {
    g[i] <- (a[i] > 0.3)
  }
  sum(g)
}

# Perform benchmark.
microbenchmark::microbenchmark(slow(a), sum(a>0.3))

### EXERCISE 1.2

### EXERCISE 1.3

### EXERCISE 1.4

Z.1 <- rnorm(1000)
Z.2 <- rnorm(1000)
Z.3 <- rnorm(1000)

# Make it tidy.
Z <- tibble::tibble(Z.1, Z.2, Z.3)

# Estimate summaries.
Z.mu    <- purrr::map_dbl(Z, mean)
Z.sigma <- purrr::map_dbl(Z, var)

# Lognormal variance.
lognormal.var <- function(mu, sigma) {
  (exp(sigma) - 1)*exp(2*mu+sigma)
}

# Lognormal variance estimate.
P.var.trial <- sum(lognormal.var(Z.mu, Z.sigma))

# Estimate approximative amount of samples R.
R <- (qnorm(0.975)^2*P.var.trial)/(0.01^2)
R <- ceiling(R)

# Estimate P.exp
Z.1.R <- rnorm(R)
Z.2.R <- rnorm(R)
Z.3.R <- rnorm(R)

Z.R <- tibble::tibble(Z.1.R, Z.2.R, Z.3.R)

Z.mu.R    <- purrr::map_dbl(Z.R, mean)
Z.sigma.R <- purrr::map_dbl(Z.R, var)

P.exp   <- sum(exp(Z.mu.R+Z.sigma.R/2))
conf    <- qnorm(0.975) * sqrt(lognormal.var(0, 1)*3)/sqrt(R)
P.var.R <- sum(lognormal.var(Z.mu.R, Z.sigma.R))

P.var <- lognormal.var(0, 1)*3
diff <- P.var.trial - P.var

# sqrt(sum((Z.1-mean(Z.1))^2)/(length(Z.1)-1))

### EXERCISE 1.5
n <- 1e6
U <- runif(n)
Z <- 4 * sqrt(1 - U^2)
Z.exp <- mean(Z)


Z <- function(U){
  4 * sqrt(1 - U^2)
}

a <- c()

for (i in 1:(n-1)) {
  a[i] <- (Z(U[i]))
}
suma <- sum(a)


trap <- ((1/n)/2) * (Z(U[1]) + Z(U[n]) + 2*suma)
