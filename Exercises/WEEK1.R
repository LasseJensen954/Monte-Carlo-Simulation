#### ANDREAS KRACHT FRANDSEN.

### EXERCISE 1.1.
library(Rcpp)

# Generate uniform RV.
a <- runif(1e6)

# Make slow function.
slow <- function(a) {
  g <- c()
  for (i in seq_along(a)) {
    g[i] <- (a[i] > 0.3)
  }
  sum(g)
}

cppFunction('int fastC(NumericVector x) {
  register unsigned int n = x.size();
  register unsigned int total = 0;
  for(int i = n; i--;) {
    total += x[i] > 0.3;
  }
  return total;
}')

# Perform benchmark.
microbenchmark::microbenchmark(slow(a), sum(a>0.3), fastC(a))

# Unit: microseconds
# expr              min        lq       mean   median       uq      max neval
# slow(a)      218287.9 233692.15 241861.643 236573.2 241536.9 287712.7   100
# sum(a > 0.3)   3743.8   4124.65   4926.905   4216.6   4372.0  56818.4   100
# fastC(a)        783.7    835.50    853.573    851.4    868.4    971.2   100

### EXERCISE 1.2

### EXERCISE 1.3

### EXERCISE 1.4

# Samples of normal dist.
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
  (exp(sigma) - 1) * exp(2 * mu + sigma)
}

# Lognormal variance estimate.
P.var.trial <- sum(lognormal.var(Z.mu, Z.sigma))

# Estimate approximative amount of samples R.
R <- (qnorm(0.975)^2 * P.var.trial) / (eps^2)
R <- ceiling(R)

# Estimate P.exp with R samples
Z.1.R <- rnorm(R)
Z.2.R <- rnorm(R)
Z.3.R <- rnorm(R)

Z.R <- tibble::tibble(Z.1.R, Z.2.R, Z.3.R)

Z.mu.R    <- purrr::map_dbl(Z.R, mean)
Z.sigma.R <- purrr::map_dbl(Z.R, var)

P.exp   <- sum(exp(Z.mu.R + Z.sigma.R / 2))
P.var.R <- sum(lognormal.var(Z.mu.R, Z.sigma.R))
conf    <- qnorm(0.975) * sqrt(P.var.R) / sqrt(R)

P.var  <- lognormal.var(0, 1) * 3
R.true <- (qnorm(0.975)^2 * P.var) / (eps^2)
R.true <- ceiling(R.true)

conf.true <- qnorm(0.975)*sqrt(lognormal.var(0, 1)*3)/sqrt(R.true)

# sqrt(sum((Z.1-mean(Z.1))^2)/(length(Z.1)-1))

### EXERCISE 1.5
n <- 1e6
U <- runif(n)
Z <- 4 * sqrt(1 - U^2)
Z.exp <- mean(Z)


# TRAP
grid <- seq(0, 1, 1/n)

Z <- function(x){
  4 * sqrt(1 - x^2)
}

Trap <- function(x, n){
  a <- c()
  for (i in 1:(n-1)) {
    a[i] <- (Z(x[i]))
  }
  ((1/n)/2) * (Z(x[1]) + Z(x[n]) + 2*sum(a))
}


# RIEMANN
grid <- seq(0, 1, 1/n)

Riemann <- function(x, n) {
  b <- c()
  for (i in 1:n) {
    b[i] <- Z(x[i])
  }
  sum(b)*(1/n)
}

